/* Project.scala
 * Coq project configuration managers and the Coq project builder
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.core.resources.{IResourceDelta, IResourceDeltaVisitor}
import org.eclipse.core.runtime.{IPath, Path, SubMonitor, IProgressMonitor}
import org.eclipse.core.resources.{IFile, IFolder, IMarker, IProject,
  IResource, IContainer, IWorkspace, IWorkspaceRunnable, IProjectDescription}
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.core.resources.IProjectNature

class CoqBuilder extends IncrementalProjectBuilder {
  import java.util.{Map => JMap}
  import CoqBuilder._
  import DependencyTracker._
  
  override protected def getRule(
      type_ : Int, args : JMap[String, String]) = getProject
  
  def loadPathProviders = getLoadPathProviders(getProject)
      
  private var deps : Option[DependencyTracker] = None
  
  private def partBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    if (deps == None)
      return fullBuild(args, monitor)
    
    var changedFiles = Set[IFile]()
      
    val delta = getDelta(getProject())
    delta.accept(new IResourceDeltaVisitor {
      override def visit(d : IResourceDelta) : Boolean = {
        TryCast[IFile](d.getResource).flatMap(extensionFilter("v")).foreach(
            f => changedFiles += f)
        true
      }
    })
    
    buildFiles(changedFiles, args, monitor)
  }
  
  private def getCorrespondingObject(s : IPath) =
    CoqBuilder.getCorrespondingObject(getProject)(s)
  
  private var completeLoadPath : Seq[(Seq[String], java.io.File)] = Seq()
  private var possibleObjects : Set[IPath] = Set()
  
  private def buildFiles(files : Set[IFile],
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    if (!CoqProgram("coqtop").check) {
      createResourceErrorMarker(getProject, "Can't find the Coq compiler")
      return Array()
    }
    val dt = deps.get
    
    /* files contains only those files which are known to have changed. Their
     * object files, and all their dependencies, must be deleted */
    var done = Set[IPath]()
    var todo = List[IPath]() ++ files.map(_.getLocation)
    while (todo.headOption != None) {
      val i = todo.head
      todo = todo.tail
      getCorrespondingObject(i).foreach(a => {
        done += i
        val p = a.getLocation
        if (a.exists)
          a.delete(IResource.NONE, null)
        dt.allDependencies.foreach(a => {
          val (path, dependencies) = a
          /* If file depended on this object, then schedule its object for
           * deletion */
          dependencies.exists(_ match {
            case (_, Some(p_)) if p == p_ =>
              todo :+= path; true
            case _ => false
          })
        })
      })
    }
    /* done now contains those files which must be built -- that is, which have
     * changed (or which have a dependency on something which changed) */
    
    /* Schedule all files with broken dependencies to be built, too (because
     * those dependencies might become satisfiable) */
    dt.allDependencies.foreach(a => {
      val (path, dependencies) = a
      if (dependencies.exists(b => b._2 == None))
        done += path
    })
    
    done = done.flatMap(
        i => (getCorrespondingObject(i), getFileForLocation(i)) match {
      case (Some(output), file) if file.exists =>
        /* This file's going to be built; create the output directory (and
         * remove any stale objects)... */
        if (output.exists) {
          if (output.getLocalTimeStamp < file.getLocalTimeStamp)
            output.delete(IWorkspace.AVOID_UPDATE, null)
        } else new FolderCreationRunner(output).run(null)
        
        /* ... clear all of the problem markers... */
        file.deleteMarkers(
            ManifestIdentifiers.MARKER_PROBLEM, true, IResource.DEPTH_ZERO)

        /* ... and forget all of the resolved dependencies */
        dt.setDependencies(i, dt.getDependencies(i).map(a => (a._1, None)))
        Some(i)
      case _ =>
        /* This file can't be built (either because it's been deleted or
         * because it has no possible output location): discard its
         * dependencies and remove it from the buid queue */
        dt.setDependencies(i, Set.empty)
        None
    })
    
    /* Recalculate the dependencies for the files which have actually
     * changed */
    files.filter(_.exists).foreach(
        i => dt.setDependencies(i.getLocation, generateDeps(i)))
    
    /* Expand the load path */
    completeLoadPath =
      loadPathProviders.flatMap(_.getLoadPath).flatMap(_.expand)
    
    possibleObjects = done.flatMap(getCorrespondingObject).map(_.getLocation)
    
    class BuildTaskImpl(src : IPath) extends BuildManager.BuildTask {
      private def canBuild = {
        dt.resolveDependencies(src)
        !dt.getDependencies(src).exists(a => a._2 == None)
      }
      import org.eclipse.core.runtime.CoreException
      override def build = if (isInterrupted || monitor.isCanceled) {
        BuildManager.BuildTask.Abandoned
      } else if (canBuild) {
        val r = new CompileCoqRunner(
          getFileForLocation(src), getCorrespondingObject(src))
        r.setTicker(Some(() => !isInterrupted() && !monitor.isCanceled))
        try {
          r.run(monitor.newChild(1, SubMonitor.SUPPRESS_NONE))
          BuildManager.BuildTask.Succeeded
        } catch {
          case c : CoreException if isInterrupted || monitor.isCanceled =>
            BuildManager.BuildTask.Abandoned
          case c : CoreException => BuildManager.BuildTask.Failed(c)
        }
      } else BuildManager.BuildTask.Waiting
      
      import BuildManager.BuildTask._
      override def cleanup = state match {
        case Waiting =>
          val f = getFileForLocation(src)
          var broken = dt.getDependencies(src).collect {
            case ((arg, _), None) => arg
          }
          if (!broken.isEmpty)
            createResourceErrorMarker(f,
              "Broken dependencies: " + broken.mkString(", ") + ".")
        case Failed(e) =>
          val f = getFileForLocation(src)
          e.getStatus.getMessage.trim match {
            case CompilationError(_, line, _, _, message) =>
              createLineErrorMarker(
                f, line.toInt, message.replaceAll("\\s+", " ").trim)
            case msg => createResourceErrorMarker(f, msg)
          }
        case Abandoned => dt.setDependencies(src,
            dt.getDependencies(src).map(a => (a._1, None)))
        case Succeeded =>
      }
    }
    
    monitor.beginTask("Compiling", done.size)
    BuildManager.buildLoop(done.map(new BuildTaskImpl(_)))

    /* Remove any unused output directories */
    cleanProject(ICoqModel.toCoqProject(getProject))

    Array()
  }
  
  private def createResourceErrorMarker(r : IResource, s : String) = {
    import scala.collection.JavaConversions._
    r.createMarker(ManifestIdentifiers.MARKER_PROBLEM).setAttributes(Map(
        (IMarker.MESSAGE, s),
        (IMarker.SEVERITY, IMarker.SEVERITY_ERROR)))
  }
  
  private def createLineErrorMarker(
      f : IFile, line : Int, s : String) = {
    import scala.collection.JavaConversions._
    f.createMarker(ManifestIdentifiers.MARKER_PROBLEM).setAttributes(Map(
        (IMarker.MESSAGE, s),
        (IMarker.LOCATION, "line " + line),
        (IMarker.LINE_NUMBER, line),
        (IMarker.SEVERITY, IMarker.SEVERITY_ERROR)))
  }
    
  private def fullBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    val dt = new DependencyTracker
    deps = Some(dt)
    
    traverse[IFile](getProject,
        a => TryCast[IFile](a).flatMap(extensionFilter("v")),
        a => dt.setDependencies(a.getLocation, generateDeps(a)))
    buildFiles(Set(), args, monitor)
  }
  
  override protected def clean(monitor : IProgressMonitor) = {
    deps = None
    getProject.deleteMarkers(
        ManifestIdentifiers.MARKER_PROBLEM, true, IResource.DEPTH_INFINITE)
    traverse[IFile](getProject,
        a => TryCast[IFile](a).flatMap(
            extensionFilter("vo")).flatMap(derivedFilter(true)),
        a => a.delete(IResource.NONE, monitor))
  }
  
  override protected def build(kind : Int, args_ : JMap[String, String],
      monitor_ : IProgressMonitor) : Array[IProject] = {
    getProject.deleteMarkers(
        ManifestIdentifiers.MARKER_PROBLEM, true, IResource.DEPTH_ZERO)
    val monitor = SubMonitor.convert(
        monitor_, "Building " + getProject.getName, 1)
    val args = scala.collection.JavaConversions.mapAsScalaMap(args_).toMap
    try {
      val delta = getDelta(getProject())
      kind match {
        case IncrementalProjectBuilder.AUTO_BUILD if delta != null =>
          partBuild(args, monitor.newChild(1, SubMonitor.SUPPRESS_NONE))
        case IncrementalProjectBuilder.INCREMENTAL_BUILD if delta != null =>
          partBuild(args, monitor.newChild(1, SubMonitor.SUPPRESS_NONE))
        case _ =>
          fullBuild(args, monitor.newChild(1, SubMonitor.SUPPRESS_NONE))
      }
    } finally {
      monitor.done
    }
  }
  
  private def resolveLoad(t : String) : Option[IPath] = {
    val dt = deps.get
    for ((_, location) <- completeLoadPath) {
      val p = new Path(location.getAbsolutePath).
          append(t).addFileExtension("v")
      val deps = dt.getDependencies(p)
      if (!deps.isEmpty) {
        if (deps.exists(a => a._2 == None)) {
          /* This source file is the best candidate, but its dependencies
           * haven't been resolved yet, so we should try it again later (when
           * it's more likely to work properly) */
          return None
        } else return Some(p)
      }
    }
    return None
  }
  
  private def resolveRequire(t : String) : Option[IPath] = {
    val (coqdir, libname) = {
      val i = t.split('.').toSeq
      (i.init, i.last)
    }
    
    for ((coqdir, location) <- completeLoadPath) {
      val p = new Path(location.getAbsolutePath).
          append(libname).addFileExtension("vo")
      val f = p.toFile
      if (coqdir.endsWith(coqdir)) {
        if (!f.exists) {
          if (possibleObjects.contains(p))
            /* This object is the best candidate, but it doesn't exist yet,
             * so we should try it again later */
            return None
        } else return Some(p)
      }
    }
    None
  }

  private def generateDeps(file : IFile) : Set[DependencyTracker.Dep] = {
    val deps = CoqBuilder.generateDeps(file).map(_ match {
      case LoadRef(r) => (r, resolveLoad(_))
      case RequireRef(r) => (r, resolveRequire(_))
    })
    /* In order to force files whose dependencies have been recalculated to be
     * considered for compilation, every file starts with a synthetic,
     * trivially-satisfiable dependency on itself */
    val fakeDependency = ("!Dummy", (_ : String) => Some(file.getLocation()))
    (deps :+ fakeDependency).map(a => (a, None)).toSet
  }
  
  override def toString = "(CoqBuilder for " + getProject + ")"
  
  private def getFileForLocation(l : IPath) =
    getProject.getWorkspace.getRoot.getFileForLocation(l)
}
object CoqBuilder {
  private def getLibraryLocation = new Path(
    CoqProgram("coqtop").run(Seq("-where")).readAll._2)
    
  private val Load = "^Load (.*)\\.$".r
  private val Require = "^Require (Import |Export |)(.*)\\.$".r
  private val CompilationError =
    """(?s)File "(.*)", line (\d+), characters (\d+)-(\d+):(.*)$""".
        r.unanchored

  private def cleanProject(project : ICoqProject) : Unit =
    for (i <- project.getLoadPathProviders) i match {
      case SourceLoadPath(_, Some(output)) => cleanHierarchy(output)
      case DefaultOutputLoadPath(output) => cleanHierarchy(output)
      case _ =>
    }

  private def cleanHierarchy(dir : IContainer) : Unit = if (dir.exists) {
    for (i <- dir.members;
         j <- TryCast[IContainer](i))
      cleanHierarchy(j)
    if (dir.members().length == 0)
      dir.delete(IResource.NONE, null)
  }

  private def sentences(content : String) : Seq[CoqStep] =
    CoqEditorHandler.makeSteps(stripComments(content), 0, content.length)
  private def stripComments(doc : String) : String = {
    var regions : List[Substring] = List()
    var i = 0
    var regionStart = 0
    var inString = false
    var commentDepth = 0
    import CoqEditorHandler._
    while (i < doc.length) Substring(doc, i) match {
      case CommentStart() if !inString =>
        if (commentDepth == 0)
          regions :+= Substring(doc, regionStart, i)
        commentDepth += 1
        i += 2
      case CommentEnd() if !inString && commentDepth > 0 =>
        commentDepth -= 1
        if (commentDepth == 0)
          regionStart = i + 2
        i += 2
      case QuotationMark() =>
        inString = !inString
        i += 1
      case _ =>
        i += 1
    }
    (regions :+ Substring(doc, regionStart, i)).
        mkString(" ").replaceAll("\\s+", " ").trim
  }
  
  def traverse[A <: IResource](folder : IContainer,
      filter : IResource => Option[A], f : A => Unit) : Unit = {
    for (i <- folder.members(IContainer.INCLUDE_HIDDEN)) {
      filter(i).map(f)
      TryCast[IContainer](i).foreach(traverse(_, filter, f))
    }
  }
  
  def extensionFilter[A <: IResource](ext : String)(r : A) : Option[A] =
    Option(r).filter(_.getFileExtension == ext)
    
  def derivedFilter[A <: IResource](der : Boolean)(r : A) : Option[A] =
    Option(r).filter(_.isDerived == der)

  def getLoadPathProviders(p : IProject) : Seq[ICoqLoadPathProvider] = {
    val libraryLocation = getLibraryLocation
    ICoqModel.toCoqProject(p).getLoadPathProviders ++ List(
        ExternalLoadPath(libraryLocation.append("theories"), Some("Coq")),
        ExternalLoadPath(libraryLocation.append("plugins"), Some("Coq")),
        ExternalLoadPath(libraryLocation.append("user-contrib"), None))
  }
  
  def getCorrespondingObject(project : IProject)(s : IPath) : Option[IFile] = {
    for (i <- getLoadPathProviders(project)) i match {
      case SourceLoadPath(src, bin) if src.getLocation.isPrefixOf(s) =>
        val p = s.removeFirstSegments(src.getLocation.segmentCount).
          removeFileExtension.addFileExtension("vo")
        val outputFolder = bin.getOrElse(
          ICoqModel.toCoqProject(project).getDefaultOutputLocation)
        return Some(outputFolder.getFile(p))
      case _ =>
    }
    None
  }
  
  sealed abstract class CoqReference
  case class LoadRef(value : String) extends CoqReference
  case class RequireRef(value : String) extends CoqReference
  
  private def generateDeps(file : IFile) : Seq[CoqReference] = {
    var result = Seq.newBuilder[CoqReference]
    for (i <- sentences(
        FunctionIterator.lines(file.getContents).mkString("\n"))) {
      i.text.trim match {
        case Load(what) =>
          result += LoadRef(what)
        case Require(how, what) if what(0) == '"' =>
          val filename = what.substring(1).split("\"", 2)(0)
          result += RequireRef(filename)
        case Require(how, what) =>
          for (j <- what.split(" "))
            result += RequireRef(j)
        case _ =>
      }
    }
    result.result
  }
  
  def generateMakefile(project : IProject) : String = {
    val cp = ICoqModel.toCoqProject(project)
    val loadPath = getLoadPathProviders(project)
    val completeLoadPath = loadPath.flatMap(_.getLoadPath).flatMap(_.expand)
    def getCorrespondingObject(s : IPath) =
      CoqBuilder.getCorrespondingObject(project)(s)
    
    val dt = new DependencyTracker
    var allFiles : Set[IFile] = Set()
    traverse[IFile](project,
        a => TryCast[IFile](a).flatMap(extensionFilter("v")),
        a => allFiles += a)
    var possibleObjects : Set[IPath] = Set()
    var built : Set[IPath] = Set()
    
    def resolveLoad(t : String) : Option[IPath] = {
      for ((_, location) <- completeLoadPath) {
        val p = new Path(location.getAbsolutePath).
          append(t).addFileExtension("v")
        val deps = dt.getDependencies(p)
        if (!deps.isEmpty) {
          if (deps.exists(a => a._2 == None)) {
            /* This source file is the best candidate, but its dependencies
             * haven't been resolved yet, so we should try it again later (when
             * it's more likely to work properly) */
            return None
          } else return Some(p)
        }
      }
      return None
    }

    def resolveRequire(t : String) : Option[IPath] = {
      val (coqdir, libname) = {
        val i = t.split('.').toSeq
        (i.init, i.last)
      }

      for ((coqdir, location) <- completeLoadPath) {
        val p = new Path(location.getAbsolutePath).
          append(libname).addFileExtension("vo")
        val f = p.toFile
        if (coqdir.endsWith(coqdir)) {
          if (!f.exists && !built.contains(p)) {
            if (possibleObjects.contains(p))
              /* This object is the best candidate, but it doesn't exist yet,
               * so we should try it again later */
              return None
          } else return Some(p)
        }
      }
      None
    }
    
    possibleObjects = allFiles.flatMap(a => {
      val l = a.getLocation
      val fakeDependency = ("!Dummy", (_ : String) => Some(a.getLocation()))
      val deps : Set[DependencyTracker.Dep] =
          (fakeDependency +: generateDeps(a).map(_ match {
        case LoadRef(r) => (r, resolveLoad(_))
        case RequireRef(r) => (r, resolveRequire(_))
      })).map(a => (a, None)).toSet
      dt.setDependencies(l, deps)
      
      getCorrespondingObject(l).map(_.getLocation)
    })
    
    def makeLocationRelative(l : IPath) = {
      val p = project.getLocation
      if (p.isPrefixOf(l)) {
        Some(l.setDevice(null).removeFirstSegments(p.segmentCount))
      } else None
    }
    
    var sb = new StringBuilder
    class BuildTaskImpl(src : IPath) extends BuildManager.BuildTask {
      private def canBuild = {
        dt.resolveDependencies(src, true)
        !dt.getDependencies(src).exists(a => a._2 == None)
      }
      override def build() = if (canBuild) {
        val cf = getCorrespondingObject(src).get
        sb ++= cf.getProjectRelativePath + ": " +
            /* Skip all dependencies with absolute paths */
            dt.getDependencies(src).flatMap(_._2).flatMap(
                makeLocationRelative).mkString(" ") + "\n"
        built += cf.getLocation
        BuildManager.BuildTask.Succeeded
      } else BuildManager.BuildTask.Waiting
      
      override def cleanup = ()
      
      override def toString =
        "(generateMakefileDeps.BuildTaskImpl for " + src + ")"
    }
    
    sb ++= "override _COQCMD = \\\n\t" +
        """mkdir -p "`dirname "$@"`" && coqc $(COQFLAGS) "$<" && mv "$<o" "$@" """ +
        "\nCOQFLAGS = -noglob\n\n"
    
    for (i <- cp.getLoadPathProviders) {
      i match {
        case SourceLoadPath(src, bin_) =>
          val bin = bin_.getOrElse(cp.getDefaultOutputLocation)
          sb ++= bin.getProjectRelativePath + "/%.vo: " +
              src.getProjectRelativePath + "/%.v\n\t$(_COQCMD)\n"
        case _ =>
      }
      for (j <- i.getLoadPath)
        sb ++= "override COQFLAGS += -R \"" +
            makeLocationRelative(j.path).getOrElse(j.path) + "\" \"" +
            j.coqdir.getOrElse("") + "\"\n"
    }
    
    sb ++= "OBJECTS = " + allFiles.flatMap(
        a => getCorrespondingObject(a.getLocation).map(_.getProjectRelativePath)).
            mkString("\\\n\t", " \\\n\t", "")
    sb ++= "\n\nall: $(OBJECTS)\nclean:\n\trm -f $(OBJECTS)\n\n"
        
    BuildManager.buildLoop(allFiles.map(a => new BuildTaskImpl(a.getLocation)))
    sb.result
  }
}

class Substring(base : CharSequence, start : Int, end : Int)
    extends CharSequence with Seq[Char] {
  private class SubstringIterator extends Iterator[Char] {
    private var position = 0
    override def hasNext = (position < Substring.this.length)
    override def next = try charAt(position) finally position = position + 1
  }
  
  override def apply(offset : Int) = charAt(offset)
  override def charAt(offset : Int) = base.charAt(start + offset)
  override def length = end - start
  override def subSequence(start : Int, end : Int) =
    new Substring(this, start, end)
  
  override def iterator : Iterator[Char] = new SubstringIterator
  
  override def toString = mkString
}
object Substring {
  def apply(base : CharSequence) : Substring =
    apply(base, 0, base.length)
  def apply(base : CharSequence, start : Int) : Substring =
    apply(base, start, base.length)
  def apply(base : CharSequence, start : Int, end : Int) : Substring =
    new Substring(base, start, end)
}

class DependencyTracker {
  import DependencyTracker._

  private var deps = Map[IPath, Set[Dep]]()

  def addDependency(from : IPath, to : Dep) : Unit =
    deps = deps + (from -> (getDependencies(from) + to))
  def setDependencies(from : IPath, to : Set[Dep]) =
    deps = deps + (from -> to)

  def resolveDependencies(file : IPath, debug : Boolean = false) : Boolean = {
    var resolution = false
    setDependencies(file, getDependencies(file).map(_ match {
      case d @ (_, Some(_)) => /* do nothing */ d
      case (p @ (identifier, resolver), q) =>
        val r = resolver(identifier)
        if (r != q)
          resolution = true
        (p, r)
    }))
    resolution
  }

  def getDependencies(from : IPath) : Set[Dep] = deps.getOrElse(from, Set())

  def allDependencies = deps.iterator
  
  override def toString = allDependencies.map(
      a => (Seq(a._1.toPortableString) ++ a._2.map(depToString)).mkString("\n\t\t")).
          mkString("DG\n\t", "\n\t", "")
}
object DependencyTracker {
  type DepCallback = (String, String => Option[IPath])
  type Dep = (DepCallback, Option[IPath])
  
  def depToString(d : Dep) : String = d match {
    case ((ident, _), None) => "[X] " + ident
    case (_, Some(p)) => "[O] " + p.toPortableString
  }
}

class FunctionIterator[A](f : () => Option[A]) extends Iterator[A] {
  private var cache : Option[A] = None
  private def prepare() : Unit = cache = cache.orElse { f() }
  override def next() = { prepare(); try cache.get finally cache = None }
  override def hasNext() = { prepare(); (cache != None) }
}
object FunctionIterator {
  def apply[A](f : () => A) = new FunctionIterator(() => Option(f()))
  import java.io.{Reader, InputStream, BufferedReader, InputStreamReader}
  def lines(i : InputStream) : FunctionIterator[String] =
    lines(new InputStreamReader(i))
  def lines(i : Reader) : FunctionIterator[String] =
    apply(new BufferedReader(i).readLine)
}

class FolderCreationRunner(a : IResource) extends JobRunner[Unit] {
  private def create(a : IFolder) : Unit = {
    if (a.exists)
      return
    TryCast[IFolder](a.getParent).foreach(create)
    a.create(IResource.NONE, true, null)
  }
  
  override def doOperation(monitor : SubMonitor) : Unit =
    TryCast[IFolder](a.getParent).foreach(create)
}

object TryCast {
  def apply[A](a : Any)(implicit a0 : Manifest[A]) = a0.unapply(a)
}

class CoqNature extends IProjectNature {
  import org.eclipse.core.resources.ICommand
  
  private var project : IProject = null
  
  override def setProject(project : IProject) = {
    this.project = project
  }
  
  override def getProject = project
  
  override def configure = {
    project.setDescription(
        ICoqProject.configureDescription(project.getDescription), null)
  }
  
  override def deconfigure = {
    project.setDescription(
        ICoqProject.deconfigureDescription(project.getDescription), null)
  }
}