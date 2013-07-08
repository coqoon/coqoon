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
  import CoqBuilder._
  
  def loadPath : Seq[ICoqLoadPath] =
      ICoqModel.forProject(getProject).getLoadPath
        
  private val coqTop = CoqTopIdeSlave_v20120710()
  
  private var deps : Option[DependencyGraph] = None
  
  private def partBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    println("Part build for " + getProject)
    if (deps == None)
      return fullBuild(args, monitor)
    
    var changedFiles = Set[IFile]()
      
    val delta = getDelta(getProject())
    delta.accept(new IResourceDeltaVisitor {
      override def visit(d : IResourceDelta) : Boolean = {
        Option(d.getResource).flatMap(
            fileFilter).flatMap(extensionFilter("v")) match {
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
  
  private def buildFiles(files : Set[IFile],
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    println(this + ".buildFiles(" + files + ", " + args + ", " + monitor + ")")
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
          dg.dependencySet.foreach(_ match {
            case (f, ((_, _), Some(p_))) if p == p_ =>
              /* f depended on this object; schedule its object for deletion */
              todo :+= f
            case _ =>
          })
        })
      }
    }
    /* done now contains those files which must be built -- that is, which have
     * changed (or which have a dependency on something which changed) */
    
    /* Schedule all files with broken dependencies to be built, too (because
     * those dependencies might become satisfiable) */
    dg.dependencySet.foreach(_ match {
      case (file, (_, None)) =>
        done += file
      case _ =>
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
        i.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)

        /* ... and forget all of the resolved dependencies */
        dg.setDependencies(i, dg.getDependencies(i).map(_ match {
          case (f, _) => (f, None)
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
      
    var failureCount = 0
    var failed : Option[List[IFile]] = None
    todo = done.toList
    done = Set()
    while (todo.headOption != None && !monitor.isCanceled) {
      val i = todo.head
      todo = todo.tail
      if (!done.contains(i)) {
        val deps = dg.getDependencies(i)
        if (!deps.exists(a => a._2 == None)) {
          /* Attempt to compile files whose dependencies are all satisfied */
          println("Deps are OK     for " + i + ", attempting compilation")
          try {
            new CoqCompileRunner(i, getCorrespondingObject(i)).run(
                monitor.newChild(1, SubMonitor.SUPPRESS_NONE))
          } catch {
            case e : org.eclipse.core.runtime.CoreException =>
              e.getStatus.getMessage.trim match {
                case CompilationError(_, line, _, _, message) =>
                  createLineErrorMarker(
                      i, line.toInt, message.replaceAll("\\s+", " ").trim)
                case msg => createFileErrorMarker(i, msg)
              }
          }
          failureCount = 0
          done = done + i
        } else {
          /* Prepare files for compilation by trying to resolve their broken
           * dependencies */
          println("Deps are BROKEN for " + i + ", attempting resolution")
          if (dg.resolveDependencies(i)) {
            failureCount = 0
          } else failureCount = failureCount + 1
          todo :+= i
        }
      }
      /* If we've been through the entire build queue without being able to
       * resolve any more dependencies, then give up */
      if (todo.size > 0 && failureCount == todo.size) {
        println("Aieee, everything's failed, giving up")
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
          case (f, _) => (f, None)
        }))
    
    /* Create error markers for files with broken dependencies */
    for (a <- failed; b <- a) {
      val dps = dg.getDependencies(b).collect(_ match {
        case ((arg, _), None) => arg
      })
      createFileErrorMarker(b,
          "Broken dependencies: " + dps.mkString(" ") + ".")
    }
    
    Array()
  }
  
  private def createFileErrorMarker(f : IFile, s : String) = {
    import scala.collection.JavaConversions._
    f.createMarker(IMarker.PROBLEM).setAttributes(Map(
        (IMarker.MESSAGE, s),
        (IMarker.SEVERITY, IMarker.SEVERITY_ERROR)))
  }
  
  private def createLineErrorMarker(
      f : IFile, line : Int, s : String) = {
    import scala.collection.JavaConversions._
    f.createMarker(IMarker.PROBLEM).setAttributes(Map(
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
  
  private def fileFilter(r : IResource) : Option[IFile] = TryCast[IFile](r)
  
  private def extensionFilter[A <: IResource](ext : String)(r : A) : Option[A] =
    Option(r).filter(_.getFileExtension == ext)
    
  private def derivedFilter[A <: IResource](der : Boolean)(r : A) : Option[A] =
    Option(r).filter(_.isDerived == der)
    
  private def fullBuild(
      args : Map[String, String], monitor : SubMonitor) : Array[IProject] = {
    println("Full build for " + getProject)
    val dg = new DependencyGraph
    deps = Some(dg)
    
    traverse[IFile](getProject,
        a => Option(a).flatMap(fileFilter).flatMap(extensionFilter("v")),
        a => dg.setDependencies(a, generateDeps(a)))
    buildFiles(Set(), args, monitor)
  }
  
  override protected def clean(monitor : IProgressMonitor) = {
    coqTop.map(_.kill)
    deps = None
    traverse[IFile](getProject,
        a => Option(a).flatMap(fileFilter).flatMap(
            extensionFilter("vo")).flatMap(derivedFilter(true)),
        a => a.delete(IResource.NONE, monitor))
  }
  
  override protected def build(kind : Int,
      args_ : java.util.Map[String, String],
      monitor_ : IProgressMonitor) : Array[IProject] = {
    val monitor = SubMonitor.convert(
        monitor_, "Building " + getProject.getName, 1)
    val args = scala.collection.JavaConversions.asScalaMap(args_).toMap
    try {
      println(this + ".build(" + kind + ", " + args + ", " + monitor + ")")
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
    println("DependencyGraph.resolveLoad(" + t +
      ") (in project " + getProject + ")")
    None
  }
  private def resolveRequire(
      t : String) : Option[IPath] = coqTop.flatMap(ct => {
    for (i <- loadPath)
      ct.interp(true, false, i.asCommand)
    ct.interp(true, false, "Locate File \"" +
      t.reverse.split("\\.")(0).reverse + ".vo\".") match {
      case CoqTypes.Good(msg) =>
        Some(new Path(msg))
      case CoqTypes.Fail(_) => None
    }
  })

  private def generateDeps(file : IFile) : Set[DependencyGraph.Dep] = {
    var result : Set[DependencyGraph.Dep] = Set()
    for (
      i <- sentences(
        FunctionIterator.lines(file.getContents).mkString("\n"))
    ) {
      i.text.trim match {
        case Load(what) =>
          result += (((what, resolveLoad), None))
        case Require(how, what) =>
          if (what(0) == '"') {
            val filename = what.substring(1).split("\"", 2)(0)
            result += (((filename, resolveRequire), None))
          } else {
            for (j <- what.split(" "))
              result += (((j, resolveRequire), None))
          }
        case _ =>
      }
    }
    result
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
    var regions : List[String] = List()
    var i = 0
    var regionStart = 0
    var inString = false
    var commentDepth = 0
    while (i < doc.length) {
      if (!inString &&
        (i < doc.length - 2) && doc.substring(i, i + 2) == "(*") {
        if (commentDepth == 0) {
          val s = doc.substring(regionStart, i).trim
          if (s.length > 0)
            regions :+= s
        }
        commentDepth += 1
        i += 2
      } else if (!inString &&
        (i < doc.length - 2) && doc.substring(i, i + 2) == "*)" &&
        commentDepth > 0) {
        commentDepth -= 1
        if (commentDepth == 0)
          regionStart = i + 2
        i += 2
      } else if (commentDepth == 0) {
        if (doc(i) == '"')
          inString = !inString
        i += 1
      } else i += 1
    }
    if (commentDepth == 0) {
      val s = doc.substring(regionStart, i).trim
      if (s.length > 0)
        regions :+= s
    }
    regions.mkString(" ").replaceAll("\\s+", " ").trim
  }
}

class DependencyGraph {
  import DependencyGraph._

  private var deps = Set[(IFile, Dep)]()

  def addDependency(from : IFile, to : Dep) : Unit = (deps += from -> to)
  def setDependencies(file : IFile, to : Set[Dep]) =
    deps = deps.filterNot(_._1 == file) ++ to.map(a => file -> a)

  def resolveDependencies(file : IFile) : Boolean = {
    var resolution = false
    deps = deps.map(_ match {
      case (file_, ((identifier, resolver), None)) if file == file_ =>
        val r = resolver(identifier)
        if (r != None)
          resolution = true
        (file_, ((identifier, resolver), r))
      case d => d
    })
    resolution
  }

  def getDependencies(from : IFile) : Set[Dep] =
    deps.filter(_._1 == from).map(_._2)

  def dependencySet = deps
}
object DependencyGraph {
  type DepCallback = (String, String => Option[IPath])
  type Dep = (DepCallback, Option[IPath])
}

class FunctionIterator[A](f : () => A) extends Iterator[A] {
  private var cache : Option[A] = None
  private def prepare() : Unit = cache = cache.orElse { Option(f()) }
  override def next() = { prepare(); try cache.get finally cache = None }
  override def hasNext() = { prepare(); (cache != None) }
}
object FunctionIterator {
  def apply[A](f : () => A) = new FunctionIterator(f)
  import java.io.{InputStream, BufferedReader, InputStreamReader}
  def lines(i : InputStream) =
    apply(new BufferedReader(new InputStreamReader(i)).readLine)
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