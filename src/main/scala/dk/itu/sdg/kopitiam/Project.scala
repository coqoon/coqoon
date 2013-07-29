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
  import DependencyGraph._
  
  private def getLibraryLocation = new Path(
    CoqProgram("coqtop").run(Seq("-where"), false).readAll._2)
  
  override protected def getRule(
      type_ : Int, args : JMap[String, String]) = getProject
  
  def loadPath : Seq[ICoqLoadPath] = {
    val libraryLocation = getLibraryLocation
    ICoqModel.forProject(getProject).getLoadPath :+
      ExternalLoadPath(libraryLocation.append("theories"), Some("Coq")) :+
      ExternalLoadPath(libraryLocation.append("plugins"), Some("Coq"))
  }
      
  private var deps : Option[DependencyGraph] = None
  
  private def partBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    if (deps == None)
      return fullBuild(args, monitor)
    
    var changedFiles = Set[IFile]()
      
    val delta = getDelta(getProject())
    delta.accept(new IResourceDeltaVisitor {
      override def visit(d : IResourceDelta) : Boolean = {
        Option(d.getResource).flatMap(
            TryCast[IFile]).flatMap(extensionFilter("v")) match {
          case Some(f) => changedFiles += f
          case _ =>
        }
        true
      }
    })
    
    buildFiles(changedFiles, args, monitor)
  }
  
  private def getCorrespondingObject(s : IFile) : Option[IFile] = {
    for (i <- loadPath) i match {
      case ProjectSourceLoadPath(src, bin) if src.contains(s) =>
        val p = s.getProjectRelativePath.removeFirstSegments(
            src.getProjectRelativePath.segmentCount).
            removeFileExtension.addFileExtension("vo")
        val outputFolder = bin.map(_.folder).getOrElse(
            ICoqModel.forProject(getProject).getDefaultOutputLocation)
        return Some(outputFolder.getFile(p))
      case _ =>
    }
    None
  }
  
  import java.io.File
  private case class LPEntry(coqdir : Seq[String], location : File)
  private var completeLoadPath : Seq[LPEntry] = Seq()
  private var possibleObjects : Set[IPath] = Set()
  
  private def buildFiles(files : Set[IFile],
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    if (!CoqProgram("coqtop").check) {
      createResourceErrorMarker(getProject, "Can't find the Coq compiler")
      return Array()
    }
    val dg = deps.get
    
    /* files contains only those files which are known to have changed. Their
     * object files, and all their dependencies, must be deleted */
    var done = Set[IFile]()
    var todo = List[IFile]() ++ files
    while (todo.headOption != None) {
      val i = todo.head
      todo = todo.tail
      if (!done.contains(i)) {
        done += i
        getCorrespondingObject(i).foreach(a => {
          val p = a.getLocation
          if (a.exists)
            a.delete(IWorkspace.AVOID_UPDATE, null)
          dg.allDependencies.foreach(a => {
            val (file, dependencies) = a
            /* If file depended on this object, then schedule its object for
             * deletion */
            dependencies.exists(_ match {
              case (_, Resolved(p_)) if p == p_ => todo :+= file; true
              case (_, Resolvable(p_)) if p == p_ => todo :+= file; true
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
    dg.allDependencies.foreach(a => {
      val (file, dependencies) = a
      if (dependencies.exists(b => !b._2.isInstanceOf[Resolved]))
        done += file
    })
    
    done = done.flatMap(i => {
      if (i.exists) {
        /* This file's going to be built; create the output directory (and
         * remove any stale objects)... */
        getCorrespondingObject(i).foreach(a => {
          if (a.exists) {
            if (a.getLocalTimeStamp < i.getLocalTimeStamp)
              a.delete(IWorkspace.AVOID_UPDATE, null)
          } else new FolderCreationRunner(a).run(null)
        })

        /* ... clear all of the problem markers... */
        i.deleteMarkers(KopitiamMarkers.Problem.ID, true, IResource.DEPTH_ZERO)

        /* ... and forget all of the resolved dependencies */
        dg.setDependencies(i, dg.getDependencies(i).map(_ match {
          case (f, _) => (f, Unresolved)
        }))
        Some(i)
      } else {
        /* This file has been deleted; discard its dependencies and remove it
         * from the build queue */
        dg.setDependencies(i, Set.empty)
        None
      }
    })
    
    /* Recalculate the dependencies for the files which have actually
     * changed */
    files.filter(_.exists).foreach(
        i => dg.setDependencies(i, generateDeps(i)))
    
    monitor.beginTask("Working", done.size)
    
    /* Expand the load path */
    def recurse(lp : ICoqLoadPath) : Seq[LPEntry] = {
      def _recurse(coqdir : Seq[String], f : File) : Seq[LPEntry] = {
        val l = f.listFiles
        (if (l != null) {
          l.toSeq.filter(_.isDirectory).flatMap(
            f => _recurse(coqdir :+ f.getName, f))
        } else Seq.empty) :+ LPEntry(coqdir, f)
      }
      _recurse(
        lp.coqdir.map(_.split('.').toSeq).getOrElse(Seq()), lp.path.toFile)
    }
    completeLoadPath = loadPath.flatMap(recurse)
    
    possibleObjects = done.flatMap(getCorrespondingObject).map(_.getLocation)
    
    var failureCount = 0
    var failed : Option[List[IFile]] = None
    todo = done.toList
    done = Set()
    while (todo.headOption != None && !monitor.isCanceled) {
      val i = todo.head
      todo = todo.tail
      if (!done.contains(i)) {
        val deps = dg.getDependencies(i)
        if (!deps.exists(a => !a._2.isInstanceOf[Resolved])) {
          /* Attempt to compile files whose dependencies are all satisfied */
          try {
            new CoqCompileRunner(i, getCorrespondingObject(i)).run(
                monitor.newChild(1, SubMonitor.SUPPRESS_NONE))
          } catch {
            case e : org.eclipse.core.runtime.CoreException =>
              e.getStatus.getMessage.trim match {
                case CompilationError(_, line, _, _, message) =>
                  createLineErrorMarker(
                      i, line.toInt, message.replaceAll("\\s+", " ").trim)
                case msg => createResourceErrorMarker(i, msg)
              }
          }
          failureCount = 0
          done = done + i
        } else {
          /* Prepare files for compilation by trying to resolve their broken
           * dependencies */
          if (dg.resolveDependencies(i)) {
            failureCount = 0
          } else failureCount = failureCount + 1
          todo :+= i
        }
      }
      /* If we've been through the entire build queue without being able to
       * resolve any more dependencies, then give up */
      if (todo.size > 0 && failureCount == todo.size) {
        failed = Some(todo)
        todo = List()
      }
    }
    
    if (monitor.isCanceled)
      /* Intentionally unresolve the dependencies of all the files that were
       * awaiting compilation, thereby forcing them to be added to the next
       * compilation run */
      for (i <- todo)
        dg.setDependencies(i, dg.getDependencies(i).map(_ match {
          case (f, _) => (f, Unresolved)
        }))
    
    /* Create error markers for files with broken dependencies */
    for (a <- failed; b <- a) {
      var missing = Set[String]()
      var broken = Set[String]()
      dg.getDependencies(b).foreach(_ match {
        case ((arg, _), Unresolved) =>
          missing += arg
        case ((arg, _), Resolvable(b)) =>
          broken += arg
        case _ =>
      })
      if (!missing.isEmpty)
        createResourceErrorMarker(b,
            "Missing dependencies: " + missing.mkString(", ") + ".")
      if (!broken.isEmpty)
        createResourceErrorMarker(b,
            "Broken dependencies: " + broken.mkString(", ") + ".")
    }
    
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
  
  private def traverse[A <: IResource](folder : IContainer,
      filter : IResource => Option[A], f : A => Unit) : Unit = {
    for (i <- folder.members(IContainer.INCLUDE_HIDDEN)) {
      filter(i).map(f)
      TryCast[IContainer](i).foreach(traverse(_, filter, f))
    }
  }
  
  private def extensionFilter[A <: IResource](ext : String)(r : A) : Option[A] =
    Option(r).filter(_.getFileExtension == ext)
    
  private def derivedFilter[A <: IResource](der : Boolean)(r : A) : Option[A] =
    Option(r).filter(_.isDerived == der)
    
  private def fullBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    val dg = new DependencyGraph
    deps = Some(dg)
    
    traverse[IFile](getProject,
        a => Option(a).flatMap(TryCast[IFile]).flatMap(extensionFilter("v")),
        a => dg.setDependencies(a, generateDeps(a)))
    buildFiles(Set(), args, monitor)
  }
  
  override protected def clean(monitor : IProgressMonitor) = {
    deps = None
    getProject.deleteMarkers(
        KopitiamMarkers.Problem.ID, true, IResource.DEPTH_INFINITE)
    traverse[IFile](getProject,
        a => Option(a).flatMap(TryCast[IFile]).flatMap(
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
  
  private def resolveLoad(t : String) : DependencyStatus = Unresolved
  private def resolveRequire(t : String) : DependencyStatus = {
    val (coqdir, libname) = {
      val i = t.split('.').toSeq
      (i.init, i.last)
    }
    
    for (lp <- completeLoadPath) {
      val p = new Path(
          lp.location.getAbsolutePath).append(libname).addFileExtension("vo")
      val f = p.toFile
      if (lp.coqdir.endsWith(coqdir)) {
        if (!f.exists) {
          if (possibleObjects.contains(p))
            /* This object is the best candidate, but it doesn't exist yet,
             * so we should try again later */
            return Resolvable(p)
        } else return Resolved(p)
      }
    }
    Unresolved
  }

  private def generateDeps(file : IFile) : Set[DependencyGraph.Dep] = {
    /* In order to force files whose dependencies have been recalculated to be
     * considered for compilation, every file starts with a synthetic,
     * trivially-satisfiable dependency on itself */
    var result : Set[DependencyGraph.DepCallback] = Set(
        ("!Dummy", _ => Resolvable(file.getLocation)))
    for (i <- sentences(
        FunctionIterator.lines(file.getContents).mkString("\n"))) {
      i.text.trim match {
        case Load(what) =>
          result += ((what, resolveLoad))
        case Require(how, what) =>
          if (what(0) == '"') {
            val filename = what.substring(1).split("\"", 2)(0)
            result += ((filename, resolveRequire))
          } else {
            for (j <- what.split(" "))
              result += ((j, resolveRequire))
          }
        case _ =>
      }
    }
    result.map(a => (a, Unresolved))
  }
  
  override def toString = "(CoqBuilder for " + getProject + ")"
}
object CoqBuilder {
  final val BUILDER_ID = "dk.itu.sdg.kopitiam.CoqBuilder"
  
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

class DependencyGraph {
  import DependencyGraph._

  private var deps = Map[IFile, Set[Dep]]()

  def addDependency(from : IFile, to : Dep) : Unit =
    deps = deps + (from -> (getDependencies(from) + to))
  def setDependencies(from : IFile, to : Set[Dep]) =
    deps = deps + (from -> to)

  def resolveDependencies(file : IFile) : Boolean = {
    var resolution = false
    setDependencies(file, getDependencies(file).map(_ match {
      case d @ (_, Resolved(_)) => /* do nothing */ d
      case (p @ (identifier, resolver), q) =>
        val r = q match {
          case Unresolved => resolver(identifier)
          case r : Resolvable => r.tryResolve
        }
        if (r != q)
          resolution = true
        (p, r)
    }))
    resolution
  }

  def getDependencies(from : IFile) : Set[Dep] = deps.getOrElse(from, Set())

  def allDependencies = deps.iterator
}
object DependencyGraph {
  abstract class DependencyStatus
  case object Unresolved extends DependencyStatus
  case class Resolved(location : IPath) extends DependencyStatus
  case class Resolvable(location : IPath) extends DependencyStatus {
    def tryResolve = if (location.toFile.exists) Resolved(location) else this
  }
  
  type DepCallback = (String, String => DependencyStatus)
  type Dep = (DepCallback, DependencyStatus)
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

import org.eclipse.ui.INewWizard
import org.eclipse.jface.wizard.Wizard
class NewCoqProjectWizard extends Wizard with INewWizard {
  import org.eclipse.ui.dialogs.WizardNewProjectCreationPage
  
  class NewCoqProjectCreationPage
      extends WizardNewProjectCreationPage("Coq project") {
    
  }
  
  import org.eclipse.ui.IWorkbench
  import org.eclipse.jface.viewers.IStructuredSelection
  private var workbench : IWorkbench = null
  private var selection : IStructuredSelection = null
  override def init(w : IWorkbench, s : IStructuredSelection) = {
    workbench = w
    selection = s
  }
  
  private val creationPage = new NewCoqProjectCreationPage()
  
  override def addPages = {
    addPage(creationPage)
  }
  
  class ProjectCreator(private val project : ICoqProject)
      extends IRunnableWithProgress {
    import org.eclipse.core.runtime.SubMonitor
    import org.eclipse.core.resources.{IWorkspace, IWorkspaceRunnable}
    import org.eclipse.ui.ide.undo.WorkspaceUndoUtil
    
    private class DerivedRunnable(r : IResource) extends IWorkspaceRunnable {
      override def run(monitor : IProgressMonitor) = {
        r.setHidden(true)
        r.setDerived(true, monitor)
      }
    }
    
    override def run(monitor_ : IProgressMonitor) = {
      val monitor = SubMonitor.convert(monitor_, 4)
      
      monitor.beginTask("New Coq project", 4)
      import PathUtilities.Implicits._
      val infoAdapter = WorkspaceUndoUtil.getUIInfoAdapter(getShell)
      project.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      val src = project.getPackageFragmentRoot("src")
      src.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      val bin = project.getPackageFragmentRoot("bin")
      bin.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      
      val binFolder = bin.getCorrespondingResource.get
      val ws = binFolder.getWorkspace
      ws.run(new DerivedRunnable(binFolder),
          ws.getRuleFactory.derivedRule(binFolder),
          IWorkspace.AVOID_UPDATE, monitor.newChild(1))
    }
  }
  
  def createProject : IProject = {
    val project = creationPage.getProjectHandle()
    if (!project.exists()) {
      val mm = ICoqModel.create(project.getWorkspace.getRoot)
      getContainer().run(
          true, true, new ProjectCreator(mm.getProject(project.getName)))
    }
    return project
  }
  
  import org.eclipse.ui.PlatformUI
  
  override def performFinish = {
    val project = createProject
    PlatformUI.getWorkbench().getWorkingSetManager().addToWorkingSets(
        project, creationPage.getSelectedWorkingSets())
    true
  }
}

import org.eclipse.jface.viewers.{
  LabelProvider, ILabelProviderListener, ITreeContentProvider, Viewer}

private class LoadPathLabelProvider extends LabelProvider

private class LoadPathContentProvider extends ITreeContentProvider {
  override def dispose = ()
  
  override def inputChanged(viewer : Viewer, oldInput : Any, newInput : Any) =
    viewer.refresh
  
  override def getElements(input : Any) = input match {
    case a : ICoqProject => a.getLoadPath.toArray
    case _ => Array()
  }
  
  override def getChildren(parent : Any) = parent match {
    case a : ICoqLoadPath => Array((a.coqdir, a.path))
    case _ => Array()
  }
  
  override def getParent(child : Any) = null
  override def hasChildren(parent : Any) = true
}

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{Composite, Button, Label, TabFolder, TabItem}
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.jface.preference.PreferencePage
import org.eclipse.jface.viewers.TreeViewer

class LoadPathConfigurationPage
    extends PreferencePage with IWorkbenchPropertyPage {
  private var element : IAdaptable = null
  override def getElement : IProject = TryCast[IProject](element).get
  override def setElement(element : IAdaptable) = (this.element = element)
  override def createContents(c : Composite) = {
    val tf = new TabFolder(c, SWT.NONE)
    
    val t1 = new TabItem(tf, SWT.NONE)
    t1.setText("Source")
    val c1 = new Composite(tf, SWT.NONE)
    t1.setControl(c1)
    c1.setLayout(new FillLayout(SWT.HORIZONTAL))
    
    val c1l = new Composite(c1, SWT.NONE)
    c1l.setLayout(new FillLayout(SWT.VERTICAL))
    
    val tv = new TreeViewer(c1l)
    tv.setLabelProvider(new LoadPathLabelProvider)
    tv.setContentProvider(new LoadPathContentProvider)
    tv.setInput(ICoqModel.create(
        getElement.getWorkspace.getRoot).getProject(getElement.getName()))
    
    val c1r = new Composite(c1, SWT.NONE)
    c1r.setLayout(new FillLayout(SWT.VERTICAL))
    
    new Button(c1r, SWT.NONE).setText("I'm a button!")
    new Button(c1r, SWT.NONE).setText("I'm also a button!")
    
    val t2 = new TabItem(tf, SWT.NONE)
    t2.setText("Projects")
    
    val t3 = new TabItem(tf, SWT.NONE)
    t3.setText("Libraries")
    
    tf.pack
    tf
  }
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