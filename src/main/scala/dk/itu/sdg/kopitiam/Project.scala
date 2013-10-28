/* Project.scala
 * Coq project configuration managers and the Coq project builder
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import dk.itu.ecloq.core.coqtop.CoqProgram
import dk.itu.ecloq.core.utilities.{TryCast, Substring, FunctionIterator}

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

  private val coqProject = CacheSlot[ICoqProject] {
    ICoqModel.toCoqProject(getProject)
  }
      
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
  
  private def sourceToObject(s : IPath) =
    CoqBuilder.sourceToObject(coqProject.get)(s)
  private def objectToSource(o : IPath) =
    CoqBuilder.objectToSource(coqProject.get)(o)
  private def makePathRelative(f : IPath) =
    CoqBuilder.makePathRelative(getProject.getLocation, f)

  private var completeLoadPath : Seq[(Seq[String], java.io.File)] = Seq()
  
  private def buildFiles(files : Set[IFile],
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    if (!CoqProgram("coqtop").check) {
      createResourceErrorMarker(getProject, "Can't find the Coq compiler")
      return Array()
    }
    val dt = deps.get
    
    /* Delete any objects in the output folders that don't have a corresponding
     * source file */
    traverse[IFile](getProject,
        a => TryCast[IFile](a).flatMap(extensionFilter("vo")).filter(
            f => objectToSource(f.getLocation).size == 0),
        a => a.delete(IResource.NONE, null))
    
    /* Recalculate the dependencies for all of the files that have changed (if
     * those files are actually buildable in this project) */
    for (i <- files;
         j <- sourceToObject(i.getLocation)) {
      if (i.exists) {
        dt.setDependencies(j, generateDeps(i))
      } else dt.clearDependencies(j)
      dt.unresolveDependenciesUpon(i.getLocation, j)
    }

    /* Pre-create all of the possible output directories so that the complete
     * load path is actually complete */
    for ((path, _) <- dt.getDependencies;
         file <- makePathRelative(path).map(getProject.getFile))
      new FolderCreationRunner(file).run(null)
    completeLoadPath = coqProject.get.getLoadPath.flatMap(_.expand)

    getProject.deleteMarkers(
        ManifestIdentifiers.MARKER_PROBLEM, true, IResource.DEPTH_INFINITE)

    def isUpToDate(path : IPath) : Boolean = {
      val f = path.toFile
      if (f.exists) {
        val lm = f.lastModified
        dt.getDependencies(path).flatMap(_._3).forall(
            d => isUpToDate(d) && d.toFile.lastModified < lm)
      } else false
    }

    def canBuild(path : IPath) =
      dt.getDependencies(path).flatMap(_._3).forall(isUpToDate)
    def mustBuild(path : IPath) = {
      val lm = path.toFile.lastModified
      dt.getDependencies(path).flatMap(_._3).exists(_.toFile.lastModified > lm)
    }

    class BuildTask(val out : IPath) {
      import org.eclipse.core.runtime.CoreException

      def run(monitor : SubMonitor) = try {
        import org.eclipse.core.runtime.{IStatus, CoreException}
        objectToSource(out) match {
          case in :: Nil =>
            val inF = makePathRelative(in).map(getProject.getFile)
            val outF = makePathRelative(out).map(getProject.getFile)
            val runner = new CompileCoqRunner(inF.get, outF)
            runner.setTicker(
                Some(() => !isInterrupted && !monitor.isCanceled))
            runner.run(monitor)
          case Nil =>
            throw new CoreException(Activator.makeStatus(
                IStatus.ERROR, "Not enough source files for " + out))
          case _ =>
            throw new CoreException(Activator.makeStatus(
                IStatus.ERROR, "Too many source files for " + out))
        }
      } catch {
        case e : CoreException
            if isInterrupted || monitor.isCanceled => /* do nothing */
        case e : CoreException =>
          val f = objectToSource(out).flatMap(
              makePathRelative).map(getProject.getFile)
          (f, e.getStatus.getMessage.trim) match {
            case (f :: Nil, CompilationError(_, line, _, _, message)) =>
              createLineErrorMarker(f, line.toInt,
                message.replaceAll("\\s+", " ").trim)
            case (f :: Nil, msg) => createResourceErrorMarker(f, msg)
          }
      }
    }

    monitor.beginTask("Compiling", dt.getDependencies().size)

    var completed = Set[IPath]()
    var candidates : Seq[IPath] = Seq()
    do {
      candidates.foreach(c => new BuildTask(c).run(
          monitor.newChild(1, SubMonitor.SUPPRESS_NONE)))
      completed ++= candidates
      dt.resolveDependencies

      candidates = dt.getResolved().filter(
          a => !completed.contains(a)).partition(canBuild) match {
        case (Nil, Nil) => Nil
        case (Nil, cannot) =>
          /* This should only happen if there's a broken dependency on
           * something we can't rebuild (for example, if a file we were once
           * able to find in another project has been deleted). Re-resolve the
           * dependencies so that the error handling code below will have
           * something to show */
          dt.unresolveDependencies(cannot : _*)
          dt.resolveDependencies
          Nil
        case (can, cannot) => can.partition(mustBuild) match {
          case (need, needNot) =>
            completed ++= needNot
            monitor.worked(needNot.size)
            need
        }
      }
    } while (candidates.size != 0 && !isInterrupted && !monitor.isCanceled)

    /* Create error markers for the files that never became build candidates */
    for (i <- dt.getUnresolved;
         j :: Nil <- Some(objectToSource(i));
         f <- makePathRelative(j).map(getProject.getFile))
      createResourceErrorMarker(f, "Unresolved dependencies: " +
          dt.getDependencies(i).filter(_._3 == None).map(_._1).mkString(", "))

    /* Generate a new project makefile */
    val cp = coqProject.get
    val sb = new StringBuilder

    val orderedDeps = dt.getDependencies.keys.toSeq.sortWith(
        (a, b) => a.toPortableString.compareTo(b.toPortableString) < 0)

    sb ++= "override _COQCMD = \\\n\t" +
        """mkdir -p "`dirname "$@"`" && """ +
        """coqc $(COQFLAGS) "$<" && mv "$<o" "$@" """ +
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
            makePathRelative(j.path).getOrElse(j.path) + "\" \"" +
            j.coqdir.getOrElse("") + "\"\n"
    }

    sb ++= "OBJECTS = " + orderedDeps.flatMap(makePathRelative).
        mkString("\\\n\t", " \\\n\t", "")
    sb ++= "\n\nall: $(OBJECTS)\nclean:\n\trm -f $(OBJECTS)\n\n"

    sb ++= (
      for (out <- orderedDeps;
           deps <- Some(dt.getDependencies(out));
           r <- makePathRelative(out))
        yield r + ": " + (
          for ((_, _, Some(path)) <- deps;
               r <- makePathRelative(path))
            yield r).mkString(" ")).mkString("\n")

    val result = new java.io.ByteArrayInputStream(sb.result.getBytes("UTF-8"))
    val f = getProject.getFile("KopitiamMakefile")
    if (f.exists) {
      f.setContents(result, IResource.NONE, monitor)
    } else f.create(result, IResource.NONE, monitor)

    /* Remove any unused output directories */
    cleanProject(coqProject.get)

    coqProject.get.getLoadPathProviders.collect {
      case pl : ProjectLoadPath => pl.project
    }.toArray
  }
  
  private def createResourceErrorMarker(r : IResource, s : String) = {
    import scala.collection.JavaConversions._
    Option(r).filter(_.exists).foreach(
        _.createMarker(ManifestIdentifiers.MARKER_PROBLEM).setAttributes(Map(
            (IMarker.MESSAGE, s),
            (IMarker.SEVERITY, IMarker.SEVERITY_ERROR))))
  }
  
  private def createLineErrorMarker(
      f : IFile, line : Int, s : String) = {
    import scala.collection.JavaConversions._
    Option(f).filter(_.exists).foreach(
        _.createMarker(ManifestIdentifiers.MARKER_PROBLEM).setAttributes(Map(
            (IMarker.MESSAGE, s),
            (IMarker.LOCATION, "line " + line),
            (IMarker.LINE_NUMBER, line),
            (IMarker.SEVERITY, IMarker.SEVERITY_ERROR))))
  }
    
  private def fullBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    val dt = new DependencyTracker
    deps = Some(dt)
    
    traverse[IFile](getProject,
        a => TryCast[IFile](a).flatMap(extensionFilter("v")),
        a => sourceToObject(a.getLocation).foreach(
            b => dt.setDependencies(b, generateDeps(a))))
    buildFiles(Set(), args, monitor)
  }
  
  override protected def clean(monitor : IProgressMonitor) = {
    def deleteObjects(f : IFolder) = if (f.exists)
      traverse[IFile](f,
          a => TryCast[IFile](a).flatMap(extensionFilter("vo")),
          a => a.delete(IResource.NONE, monitor))
    deps = None
    getProject.deleteMarkers(
        ManifestIdentifiers.MARKER_PROBLEM, true, IResource.DEPTH_INFINITE)
    for (i <- coqProject.get.getLoadPathProviders) i match {
      case SourceLoadPath(src, Some(bin)) => deleteObjects(bin)
      case DefaultOutputLoadPath(bin) => deleteObjects(bin)
      case _ =>
    }
  }
  
  override protected def build(kind : Int, args_ : JMap[String, String],
      monitor_ : IProgressMonitor) : Array[IProject] = {
    /* Check that our project dependencies are in order */
    val description = getProject.getDescription
    val descriptionDependencies = description.getReferencedProjects.toSet
    val currentDependencies = coqProject.get.getLoadPathProviders.collect {
      case pl : ProjectLoadPath => pl.project
    }.toSet
    if (descriptionDependencies != currentDependencies) {
      description.setReferencedProjects(currentDependencies.toArray)
      getProject.setDescription(description, IResource.KEEP_HISTORY, monitor_)

      needRebuild()
      rememberLastBuiltState()

      return Array()
    }

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
    val (libdir, libname) = {
      val i = t.split('.').toSeq
      (i.init, i.last)
    }
    
    for ((coqdir, location) <- completeLoadPath
        if coqdir.endsWith(libdir)) {
      val p = new Path(location.getAbsolutePath).
          append(libname).addFileExtension("vo")
      val f = p.toFile
      if (f.exists || deps.get.hasDependencies(p))
        return Some(p)
    }
    None
  }

  private def generateDeps(file : IFile) : Seq[Dependency] =
    ("(self)", (_ : String) => Some(file.getLocation), Option.empty[IPath]) +:
        CoqBuilder.generateRefs(file).map(_ match {
          case LoadRef(r) => (r, resolveLoad(_), Option.empty[IPath])
          case RequireRef(r) => (r, resolveRequire(_), Option.empty[IPath])
        })
  
  override def toString = "(CoqBuilder for " + getProject + ")"
}
object CoqBuilder {
  def sourceToObject(project : ICoqProject)(location : IPath) : Option[IPath] = {
    for (i <- project.getLoadPathProviders) i match {
      case SourceLoadPath(src, bin)
          if src.getLocation.isPrefixOf(location) =>
        val base = location.removeFirstSegments(
            src.getLocation.segmentCount).removeFileExtension
        val output = bin.getOrElse(
            project.getDefaultOutputLocation).getLocation
        return Some(output.append(base).addFileExtension("vo"))
      case _ =>
    }
    None
  }
  def objectToSourceRaw(project : ICoqProject)(location : IPath) : Seq[IPath] = {
    var candidates : Seq[IPath] = Seq()
    for (i <- project.getLoadPathProviders) i match {
      case SourceLoadPath(src, bin_)
          if bin_.getOrElse(project.getDefaultOutputLocation).
              getLocation.isPrefixOf(location) =>
        val bin = bin_.getOrElse(project.getDefaultOutputLocation)
        val base = location.removeFirstSegments(
            bin.getLocation.segmentCount).removeFileExtension
        candidates :+= src.getLocation.append(base).addFileExtension("v")
      case _ =>
    }
    candidates
  }
  def objectToSource(project : ICoqProject)(location : IPath) : Seq[IPath] = {
    val handle = project.getCorrespondingResource.get
    for (i <- objectToSourceRaw(project)(location);
         j <- makePathRelative(handle.getLocation, i);
         k <- Option(handle.findMember(j)))
      yield i
  }
  def makePathRelative(base : IPath, path : IPath) : Option[IPath] =
    if (base.isPrefixOf(path)) {
      Some(path.setDevice(null).removeFirstSegments(base.segmentCount))
    } else None

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
  
  sealed abstract class CoqReference
  case class LoadRef(value : String) extends CoqReference
  case class RequireRef(value : String) extends CoqReference
  
  private def generateRefs(file : IFile) : Seq[CoqReference] = {
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