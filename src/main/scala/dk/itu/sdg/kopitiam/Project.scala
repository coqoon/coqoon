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
import org.eclipse.core.resources.{IFile, IProject,
  IResource, IContainer, IWorkspace, IWorkspaceRunnable, IProjectDescription}
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.core.resources.IProjectNature

class CoqBuilder extends IncrementalProjectBuilder {
  type BuildArgs = java.util.Map[java.lang.String, java.lang.String]
  
  def loadPath : Seq[ICoqLoadPath] = Seq(
        ExternalLoadPath(new Path("/usr/lib/coq/theories"), "Coq")) ++
        ICoqModel.forProject(getProject).getLoadPath
        
  private class DependencyGraph {
    import DependencyGraph._
    
    private var deps = Set[(IFile, Dep)]()
    
    def addDependency(from : IFile, to : Dep) : Unit = (deps += from -> to)
    def setDependencies(file : IFile, to : Set[Dep]) =
      deps = deps.filterNot(_._1 == file) ++ to.map(a => file -> a)
    
    def getDependencies(from : IFile) : Set[Dep] =
      deps.filter(_._1 == from).map(_._2)
    
    def dependencySet = deps
  }
  private object DependencyGraph {
    type DepCallback = Unit => Option[IPath]
    type Dep = Either[DepCallback, IPath]
    
    private val ct = CoqTopIdeSlave_v20120710()
    ct match {
      case Some(ct) =>
        for (i <- loadPath)
          i.run(ct)
      case None =>
    }
    
    private val Load = "^Load (.*)\\.$".r
    private val Require = "^Require (Import |Export |)(.*)\\.$".r
    
    def resolveLoad(t : String) : Option[IPath] = {
      println("DependencyGraph.resolveLoad(" + t +
          ") (in project " + getProject + ")")
      None
    }
    def resolveRequire(t : String) : Option[IPath] = {
      ct match {
        case Some(ct) =>
          ct.interp(true, false, "Locate File \"" +
              t.reverse.split("\\.")(0).reverse + ".vo\".") match {
            case CoqTypes.Good(msg) =>
              Some(new Path(msg))
            case CoqTypes.Fail(_) => None
          }
        case None => None
      }
    }
    
    def stripComments(doc : String) : String = {
      var regions : List[String] = List()
      var i = 0
      var regionStart = 0
      var result = ""
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
          result += doc(i)
          i += 1
        } else i += 1
      }
      regions.mkString(" ").replaceAll("\\s+", " ").trim
    }
    
    def sentences(content : String) : Seq[CoqStep] =
      CoqEditorHandler.makeSteps(stripComments(content), 0, content.length)
    def generateDeps(file : IFile, monitor : SubMonitor) : Set[Dep] = {
      monitor.beginTask("Calculating dependencies for " + file, 1)
      import java.io.{BufferedReader, InputStreamReader}
      val r = new BufferedReader(new InputStreamReader(file.getContents))
      var content = ""
      var c = r.readLine
      while (c != null) {
        if (monitor.isCanceled)
          return Set()
        content += c + "\n"
        c = r.readLine
      }
      var result : Set[Dep] = Set()
      for (i <- sentences(content)) {
        i.text.trim match {
          case t @ Load(what) =>
            result += Left(new Function1[Unit, Option[IPath]] {
              override def apply(a : Unit) =
                DependencyGraph.resolveLoad(what)
              override def toString =
                "DependencyGraph.resolveLoad(" + what + ")"
            })
          case t @ Require(how, what) =>
            if (what(0) == '"') {
              
            } else {
              for (j <- what.split(" ")) {
                result += Left(new Function1[Unit, Option[IPath]] {
                  override def apply(a : Unit) =
                    DependencyGraph.resolveRequire(j)
                  override def toString =
                    "DependencyGraph.resolveRequire(" + j + ")"
                })
              }
            }
          case _ =>
        }
      }
      result
    }
  }
  private var deps : Option[DependencyGraph] = None
  
  private def partBuild(
      args : BuildArgs, monitor : SubMonitor) : Array[IProject] = {
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
  
  private def contains(s : IFile, c : IContainer) : Boolean = {
    var d : IContainer = s.getParent
    while (d != null && c != d)
      d = d.getParent
    (c == d)
  }
  
  private def getCorrespondingObject(s : IFile) : Option[IFile] = {
    for (i <- loadPath) i match {
      case lp @ ProjectSourceLoadPath(src, bin)
          if contains(s, src) =>
        val p = s.getProjectRelativePath.removeFirstSegments(
            src.getProjectRelativePath.segmentCount).
            removeFileExtension.addFileExtension("vo")
        val outputDestination = bin match {
          case Some(dir) =>
            dir.folder
          case None =>
            getProject.getFolder("bin")
        }
        return Some(outputDestination.getFile(p))
      case _ =>
    }
    None
  }
  
  private def buildFiles(files : Set[IFile],
      args : BuildArgs, monitor : SubMonitor) : Array[IProject] = {
    println(this + ".buildFiles(" + files + ", " + args + ", " + monitor + ")")
    val dg = deps.get
    
    import DependencyGraph.Dep
    
    var done = Set[IFile]()
    var todo = List[IFile]() ++ files
    while (todo.headOption != None) {
      val i = todo.head
      todo = todo.tail
      if (!done.contains(i)) {
        done += i
        getCorrespondingObject(i).map(a => {
          val p = i.getFullPath
          if (a.exists)
            a.delete(IWorkspace.AVOID_UPDATE, null)
          for (i <- dg.dependencySet) i match {
            case (a, Right(p_)) if p == p_ =>
              /* a depended on this object */
              println("while rec-deps for " + i + ": must also rec-deps for " + a)
              todo :+= a
            case _ =>
          }
        })
      }
    }
    
    monitor.beginTask("Working", done.size * 2)
    
    for (i <- done)
      dg.setDependencies(i, DependencyGraph.generateDeps(i,
          monitor.newChild(1, SubMonitor.SUPPRESS_NONE)))
    
    var failureCount = 0
    todo = done.toList
    done = Set()
    while (todo.headOption != None) {
      val i = todo.head
      todo = todo.tail
      if (!done.contains(i)) {
        val deps = dg.getDependencies(i)
        if (!deps.exists(a => a.isLeft)) {
          println("Deps are OK     for " + i + ", compiling")
          /* All dependencies are resolved and compiled */
          println("All dependencies are satisfied for " + i + ", compiling")
          try {
            new CoqCompileRunner(i).run(monitor.newChild(1))
            failureCount = 0
          } catch {
            case e : org.eclipse.core.runtime.CoreException =>
              e.printStackTrace
          }
          monitor.worked(1)
          done = done + i
        } else {
          println("Deps are BROKEN for " + i + ", attempting resolution")
          /* Contains (possibly) broken dependencies: resolve them and try to
           * reschedule this for later */
          dg.setDependencies(i, deps.map(a => {
            a match {
              case l @ Left(cb) => cb() match {
                case Some(p) =>
                  println("\t" + l + " -> " + p)
                  failureCount = 0
                  Right(p)
                case None =>
                  println("\t" + l + " failed")
                  Left(cb)
              }
              case r @ Right(p) => /* already fine */ Right(p)
            }
          }))
          if (failureCount > todo.size) {
            println("Aieee, everything's failed, giving up")
            todo = List()
          } else {
            failureCount += 1
            todo :+= i
          }
        }
      } else {
        /* flag the cycle? */
      }
    }
    
    Array()
  }
  
  private def traverse[A <: IResource](folder : IContainer,
      filter : IResource => Option[A], f : A => Unit) : Unit = {
    for (i <- folder.members) {
      filter(i).map(f)
      i match {
        case i : IContainer => traverse(i, filter, f)
        case _ =>
      }
    }
  }
  
  private def fileFilter(r : IResource) : Option[IFile] = 
    if (r.isInstanceOf[IFile]) Some(r.asInstanceOf[IFile]) else None
  
  private def extensionFilter[A <: IResource](ext : String)(r : A) : Option[A] =
    Option(r).filter(_.getFileExtension == ext)
    
  private def derivedFilter[A <: IResource](der : Boolean)(r : A) : Option[A] =
    Option(r).filter(_.isDerived == der)
    
  private def fullBuild(
      args : BuildArgs, monitor : SubMonitor) : Array[IProject] = {
    println("Full build for " + getProject)
    deps = Some(new DependencyGraph)
    
    var files = Set[IFile]()
    traverse[IFile](getProject,
        a => Option(a).flatMap(fileFilter).flatMap(extensionFilter("v")),
        a => files += a)
    buildFiles(files, args, monitor)
  }
  
  override protected def clean(monitor : IProgressMonitor) = {
    deps = None
    traverse[IFile](getProject,
        a => Option(a).flatMap(fileFilter).flatMap(
            extensionFilter("v")).flatMap(derivedFilter(true)),
        a => println(a + ".delete(IResource.KEEP_HISTORY, monitor)"))
  }
  
  override protected def build(kind : Int,
      args : BuildArgs, monitor_ : IProgressMonitor) : Array[IProject] = {
    val monitor = SubMonitor.convert(monitor_, "Building " + getProject.getName, 100)
    println(this + ".build(" + kind + ", " + args + ", " + monitor + ")")
    val delta = getDelta(getProject())
    kind match {
      case IncrementalProjectBuilder.AUTO_BUILD if delta != null =>
        partBuild(args, monitor)
      case IncrementalProjectBuilder.INCREMENTAL_BUILD if delta != null =>
        partBuild(args, monitor)
      case _ =>
        fullBuild(args, monitor)
    }
  }
  
  override def toString = "(CoqBuilder for " + getProject + ")"
}

object CoqBuilder {
  final val BUILDER_ID = "dk.itu.sdg.kopitiam.CoqBuilder"
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
  override def getElement : IProject = element.asInstanceOf[IProject]
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