/* Project.scala
 * Supporting code and wizards for Coq projects and resources
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
  
  def loadPath = getLoadPath(getProject)
      
  private var deps : Option[DependencyTracker] = None
  
  private def partBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    if (deps == None)
      return fullBuild(args, monitor)
    
    var changedFiles = Set[IFile]()
      
    val delta = getDelta(getProject())
    delta.accept(new IResourceDeltaVisitor {
      override def visit(d : IResourceDelta) : Boolean = {
        TryCast[IFile](d.getResource).flatMap(extensionFilter("v")) match {
          case Some(f) => changedFiles += f
          case _ =>
        }
        true
      }
    })
    
    buildFiles(changedFiles, args, monitor)
  }
  
  private def getCorrespondingObject(s : IPath) =
    CoqBuilder.getCorrespondingObject(getProject)(s)
  
  import ICoqLoadPath._
  private var completeLoadPath : Seq[LoadPathEntry] = Seq()
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
      if (!done.contains(i)) {
        done += i
        getCorrespondingObject(i).foreach(a => {
          val p = a.getLocation
          if (a.exists)
            a.delete(IWorkspace.AVOID_UPDATE, null)
          dt.allDependencies.foreach(a => {
            val (path, dependencies) = a
            /* If file depended on this object, then schedule its object for
             * deletion */
            dependencies.exists(_ match {
              case (_, Some(p_)) if p == p_ => todo :+= path; true
              case _ => false
            })
          })
        })
      }
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
    
    done = done.flatMap(i => {
      val file = getFileForLocation(i)
      if (file.exists) {
        /* This file's going to be built; create the output directory (and
         * remove any stale objects)... */
        getCorrespondingObject(i).foreach(a => {
          if (a.exists) {
            if (a.getLocalTimeStamp < file.getLocalTimeStamp)
              a.delete(IWorkspace.AVOID_UPDATE, null)
          } else new FolderCreationRunner(a).run(null)
        })

        /* ... clear all of the problem markers... */
        file.deleteMarkers(
            KopitiamMarkers.Problem.ID, true, IResource.DEPTH_ZERO)

        /* ... and forget all of the resolved dependencies */
        dt.setDependencies(i, dt.getDependencies(i).map(_ match {
          case (f, _) => (f, None)
        }))
        Some(i)
      } else {
        /* This file has been deleted; discard its dependencies and remove it
         * from the build queue */
        dt.setDependencies(i, Set.empty)
        None
      }
    })
    
    /* Recalculate the dependencies for the files which have actually
     * changed */
    files.filter(_.exists).foreach(
        i => dt.setDependencies(i.getLocation, generateDeps(i)))
    
    /* Expand the load path */
    completeLoadPath = loadPath.flatMap(ICoqLoadPath.expand)
    
    possibleObjects = done.flatMap(getCorrespondingObject).map(_.getLocation)
    
    class BuildTaskImpl(src : IPath) extends BuildTask {
      override def canBuild = {
        dt.resolveDependencies(src)
        !dt.getDependencies(src).exists(a => a._2 == None)
      }
      override def build(monitor : SubMonitor) = {
        val f = getFileForLocation(src)
        try {
          new CoqCompileRunner(f, getCorrespondingObject(src)).run(monitor)
        } catch {
          case e : org.eclipse.core.runtime.CoreException =>
            e.getStatus.getMessage.trim match {
              case CompilationError(_, line, _, _, message) =>
                createLineErrorMarker(
                  f, line.toInt, message.replaceAll("\\s+", " ").trim)
              case msg => createResourceErrorMarker(f, msg)
            }
        }
      }
      
      override def fail = {
        val f = getFileForLocation(src)
        var broken = dt.getDependencies(src).collect {
          case ((arg, _), None) => arg
        }
        if (!broken.isEmpty)
          createResourceErrorMarker(f,
            "Broken dependencies: " + broken.mkString(", ") + ".")
      }
      
      override def forget =
        dt.setDependencies(src, dt.getDependencies(src).map(_ match {
          case (f, _) => (f, None)
        }))
    }
    
    buildLoop(monitor, done.map(new BuildTaskImpl(_)))
    Array()
  }
  
  private def createResourceErrorMarker(r : IResource, s : String) = {
    import scala.collection.JavaConversions._
    r.createMarker(KopitiamMarkers.Problem.ID).setAttributes(Map(
        (IMarker.MESSAGE, s),
        (IMarker.SEVERITY, IMarker.SEVERITY_ERROR)))
  }
  
  private def createLineErrorMarker(
      f : IFile, line : Int, s : String) = {
    import scala.collection.JavaConversions._
    f.createMarker(KopitiamMarkers.Problem.ID).setAttributes(Map(
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
        KopitiamMarkers.Problem.ID, true, IResource.DEPTH_INFINITE)
    traverse[IFile](getProject,
        a => TryCast[IFile](a).flatMap(
            extensionFilter("vo")).flatMap(derivedFilter(true)),
        a => a.delete(IResource.NONE, monitor))
  }
  
  override protected def build(kind : Int, args_ : JMap[String, String],
      monitor_ : IProgressMonitor) : Array[IProject] = {
    getProject.deleteMarkers(
        KopitiamMarkers.Problem.ID, true, IResource.DEPTH_ZERO)
    val monitor = SubMonitor.convert(
        monitor_, "Building " + getProject.getName, 1)
    val args = scala.collection.JavaConversions.asScalaMap(args_).toMap
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
  final val BUILDER_ID = "dk.itu.sdg.kopitiam.CoqBuilder"
  
  private def getLibraryLocation = new Path(
    CoqProgram("coqtop").run(Seq("-where")).readAll._2)
    
  private val Load = "^Load (.*)\\.$".r
  private val Require = "^Require (Import |Export |)(.*)\\.$".r
  private val CompilationError =
    """(?s)File "(.*)", line (\d+), characters (\d+)-(\d+):(.*)$""".
        r.unanchored
        
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
  
  trait BuildTask {
    def build(monitor : SubMonitor)
    def canBuild() : Boolean
    
    def fail()
    def forget()
  }
  
  def buildLoop(monitor : SubMonitor, toBuild_ : Set[BuildTask]) = {
    var toBuild = toBuild_
    monitor.beginTask("Compiling", toBuild.size)
    var buildable : Set[BuildTask] = Set()
    do {
      buildable.foreach(
          _.build(monitor.newChild(1, SubMonitor.SUPPRESS_NONE)))
      buildable = toBuild.filter(a => a.canBuild)
      toBuild = toBuild.filterNot(buildable.contains)
    } while (!buildable.isEmpty && !monitor.isCanceled)
    buildable.map(_.forget)
    toBuild.map(_.fail)
  }

  def getLoadPath(p : IProject) : Seq[ICoqLoadPath] = {
    val libraryLocation = getLibraryLocation
    ICoqModel.forProject(p).getLoadPath :+
      ExternalLoadPath(libraryLocation.append("theories"), Some("Coq")) :+
      ExternalLoadPath(libraryLocation.append("plugins"), Some("Coq"))
  }
  
  def getCorrespondingObject(project : IProject)(s : IPath) : Option[IFile] = {
    for (i <- getLoadPath(project)) i match {
      case ProjectSourceLoadPath(src, bin) if src.getLocation.isPrefixOf(s) =>
        val p = s.removeFirstSegments(src.getLocation.segmentCount).
          removeFileExtension.addFileExtension("vo")
        val outputFolder = bin.map(_.folder).getOrElse(
          ICoqModel.forProject(project).getDefaultOutputLocation)
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
        case Require(how, what) =>
          if (what(0) == '"') {
            val filename = what.substring(1).split("\"", 2)(0)
            result += RequireRef(filename)
          } else {
            for (j <- what.split(" "))
              result += RequireRef(j)
          }
        case _ =>
      }
    }
    result.result
  }
  
  def generateMakefile(project : IProject) : String = {
    val cp = ICoqModel.forProject(project)
    val loadPath = getLoadPath(project)
    val completeLoadPath = loadPath.flatMap(ICoqLoadPath.expand)
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
        Some(l.removeFirstSegments(p.segmentCount))
      } else None
    }
    
    var sb = new StringBuilder
    class BuildTaskImpl(src : IPath) extends BuildTask {
      override def canBuild = {
        dt.resolveDependencies(src, true)
        !dt.getDependencies(src).exists(a => a._2 == None)
      }
      override def build(monitor : SubMonitor) = {
        val cf = getCorrespondingObject(src).get
        sb ++= cf.getProjectRelativePath + ": " +
            /* Skip all dependencies with absolute paths */
            dt.getDependencies(src).flatMap(_._2).flatMap(
                makeLocationRelative).mkString(" ") + "\n"
        built += cf.getLocation
      }
      
      override def fail = ()
      override def forget = ()
      
      override def toString =
        "(generateMakefileDeps.BuildTaskImpl for " + src + ")"
    }
    
    sb ++= "override _COQCMD = \\\n\t" +
        """mkdir -p "`dirname "$@"`" && coqc $(COQFLAGS) "$<" && mv "$<o" "$@" """ +
        "\nCOQFLAGS = -noglob\n\n"
    
    for (i <- cp.getLoadPath) {
      i match {
        case ProjectSourceLoadPath(src, bin_) =>
          val bin = bin_.map(_.folder).getOrElse(cp.getDefaultOutputLocation)
          sb ++= bin.getProjectRelativePath + "/%.vo: " +
              src.getProjectRelativePath + "/%.v\n\t$(_COQCMD)\n"
        case _ =>
      }
      sb ++= "override COQFLAGS += -R \"" +
          makeLocationRelative(i.path).getOrElse(i.path) + "\" \"\"\n"
    }
    val path = cp.getDefaultOutputLocation.getLocation
    sb ++= "override COQFLAGS += -R \"" +
        makeLocationRelative(path).getOrElse(path) + "\" \"\"\n\n"
    
    sb ++= "OBJECTS = " + allFiles.flatMap(
        a => getCorrespondingObject(a.getLocation).map(_.getProjectRelativePath)).
            mkString("\\\n\t", " \\\n\t", "")
    sb ++= "\n\nall: $(OBJECTS)\nclean:\n\trm -f $(OBJECTS)\n\n"
        
    buildLoop(SubMonitor.convert(null),
        allFiles.map(a => new BuildTaskImpl(a.getLocation)))
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
      a => (Seq(a._1.toPortableString) ++ a._2.map(dep2String)).mkString("\n\t\t")).
          mkString("DG\n\t", "\n\t", "")
}
object DependencyTracker {
  type DepCallback = (String, String => Option[IPath])
  type Dep = (DepCallback, Option[IPath])
  
  implicit def dep2String(d : Dep) : String = d match {
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
  import CoqNature._
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
object CoqNature {
  final val NATURE_ID = "dk.itu.sdg.kopitiam.CoqNature"
}