/* CoqModel.scala
 * An abstraction layer between Eclipse resources and Coq concepts
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, modify, copy and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.core.commands.operations.IUndoableOperation
import org.eclipse.core.runtime.{IPath, Path, IProgressMonitor}
import org.eclipse.core.runtime.jobs.{ISchedulingRule, MultiRule}
import org.eclipse.core.resources.{
  IResource, IProjectDescription, ICommand,
  IFile, IFolder, IProject, IWorkspace, IWorkspaceRoot, IWorkspaceRunnable}

trait IParent {
  def getChildren : Seq[ICoqElement]
  def hasChildren : Boolean = (!getChildren.isEmpty)
}

sealed trait ICoqElement {
  def exists : Boolean
  def getAncestor[A]()(implicit a0 : Manifest[A]) : Option[A] =
    getParent.flatMap(TryCast[A]) match {
      case q @ Some(_) => q
      case None => getParent.flatMap(_.getAncestor[A])
    }
  def getParent : Option[ICoqElement with IParent]
  def getElementType : Class[_ <: ICoqElement]
  def getCorrespondingResource : Option[IResource]
  def getContainingResource : Option[IResource] =
    getCorrespondingResource.orElse(getParent.flatMap(_.getContainingResource))
  def getModel : ICoqModel = getAncestor[ICoqModel].get
}

private abstract class CoqElementImpl[
    A <: IResource, B <: ICoqElement with IParent](
    private val res : A, private val parent : B) extends ICoqElement {
  override def getParent = Option(parent)
  override def exists = getCorrespondingResource.exists { _.exists }
  override def getCorrespondingResource = Option(res)
}

private abstract class ParentImpl[
    A <: IResource, B <: ICoqElement with IParent](
    private val res : A, private val parent : B)
    extends CoqElementImpl(res, parent) with IParent

trait ICoqModel extends ICoqElement with IParent {
  override def getElementType = classOf[ICoqModel]
  
  override def getParent = None
  override def getCorrespondingResource : Option[IWorkspaceRoot]
  
  def getProject(name : String) : ICoqProject
  def getProjects : Seq[ICoqProject]
  def hasProjects : Boolean = (!getProjects.isEmpty)

  protected[kopitiam] def getCacheFor[A](element : ICoqElement,
      constructor : => A)(implicit a0 : Manifest[A]) : A
}
object ICoqModel {
  def create(root : IWorkspaceRoot) : ICoqModel = new CoqModelImpl(root)
  
  private val instance = new CoqModelImpl(
      org.eclipse.core.resources.ResourcesPlugin.getWorkspace.getRoot)
  def getInstance : ICoqModel = instance
  
  def forProject(project : IProject) : ICoqProject =
    getInstance.getProject(project.getName)
}

private case class CoqModelImpl(
    private val res : IWorkspaceRoot)
    extends ParentImpl(res, null) with ICoqModel {
  import CoqModelImpl._
  
  override def getProject(name : String) =
    new CoqProjectImpl(res.getProject(name), this)
  override def getProjects = res.getProjects.filter(hasNature).map(
      a => new CoqProjectImpl(a, this))
  
  override def getChildren = getProjects

  private var cache = scala.collection.mutable.Map[ICoqElement, Any]()
  override protected[kopitiam] def getCacheFor[A](
      element : ICoqElement, constructor : => A)(implicit a0 : Manifest[A]) =
    cache synchronized {
      cache.getOrElseUpdate(element, constructor).asInstanceOf[A]
    }
}
private object CoqModelImpl {
  def hasNature(a : IProject) =
    (a.getDescription.getNatureIds.exists(ICoqProject.isCoqNature))
}

final case class CoqLoadPath(path : IPath, coqdir : Option[String]) {
  def asCommand : String =
    "Add Rec LoadPath \"" + path.toOSString + "\"" + (coqdir match {
      case Some(dir) => " as " + dir
      case None => ""
    }) + "."
  def asArguments : Seq[String] =
    Seq("-R", path.toOSString, coqdir.getOrElse(""))

  import java.io.File

  def expand() : Seq[(Seq[String], File)] = {
    def _recurse(coqdir : Seq[String], f : File) : Seq[(Seq[String], File)] = {
      val l = f.listFiles
      (if (l != null) {
        l.toSeq.filter(_.isDirectory).flatMap(
          f => _recurse(coqdir :+ f.getName, f))
      } else Seq.empty) :+ (coqdir, f)
    }
    _recurse(coqdir.map(_.split('.').toSeq).getOrElse(Seq()), path.toFile)
  }
}

trait ICoqLoadPathProvider {
  def getLoadPath() : Seq[CoqLoadPath]
}

case class ProjectLoadPath(
    val project : IProject) extends ICoqLoadPathProvider {
  override def getLoadPath = ICoqModel.forProject(project).getLoadPath
}

case class SourceLoadPath(val folder : IFolder,
    val output : Option[IFolder] = None) extends ICoqLoadPathProvider {
  override def getLoadPath = List(CoqLoadPath(folder.getLocation, None)) ++
      output.map(a => CoqLoadPath(a.getLocation, None))
}

case class DefaultOutputLoadPath(
    val folder : IFolder) extends ICoqLoadPathProvider {
  override def getLoadPath = List(CoqLoadPath(folder.getLocation, None))
}

case class ExternalLoadPath(val fsPath : IPath, val dir : Option[String])
    extends ICoqLoadPathProvider {
  override def getLoadPath = List(CoqLoadPath(fsPath, dir))
}

trait ICoqProject extends ICoqElement with IParent {
  override def getElementType = classOf[ICoqProject]
  
  override def getParent : Option[ICoqModel]
  override def getCorrespondingResource : Option[IProject]
  
  def getLoadPath() : Seq[CoqLoadPath] =
    getLoadPathProviders.flatMap(_.getLoadPath)

  def getLoadPathProviders() : Seq[ICoqLoadPathProvider]
  def setLoadPathProviders(
      lp : Seq[ICoqLoadPathProvider], monitor : IProgressMonitor)
  
  def getDefaultOutputLocation : IFolder
  
  def getPackageFragmentRoot(folder : IPath) : ICoqPackageFragmentRoot
  def getPackageFragmentRoots : Seq[ICoqPackageFragmentRoot]
  def hasPackageFragmentRoots : Boolean = (!getPackageFragmentRoots.isEmpty)
  
  def getCreateOperation : IUndoableOperation
  def getDeleteOperation(deleteContent : Boolean) : IUndoableOperation
  
  override def getChildren : Seq[ICoqPackageFragmentRoot]
}
object ICoqProject {
  def isCoqNature(a : String) = (ManifestIdentifiers.NATURE_COQ == a)
  def isCoqBuilder(a : ICommand) =
    (ManifestIdentifiers.BUILDER_COQ == a.getBuilderName)
    
  def newDescription(ws : IWorkspace, name : String) : IProjectDescription =
      configureDescription(ws.newProjectDescription(name))
  
  def newDescription(proj : IProject) : IProjectDescription =
      newDescription(proj.getWorkspace, proj.getName)
  
  def configureDescription(d : IProjectDescription) :
      IProjectDescription = {
    val bs = d.getBuildSpec
    if (!bs.exists(isCoqBuilder))
      d.setBuildSpec(bs :+ makeBuilderCommand(d))
    val ns = d.getNatureIds
    if (!ns.exists(isCoqNature))
      d.setNatureIds(ns :+ ManifestIdentifiers.NATURE_COQ)
    d
  }
  
  def deconfigureDescription(d : IProjectDescription) :
      IProjectDescription = {
    d.setBuildSpec(d.getBuildSpec.filterNot(isCoqBuilder))
    d.setNatureIds(d.getNatureIds.filterNot(isCoqNature))
    d
  }
  
  def makeBuilderCommand(d : IProjectDescription) = {
    val c = d.newCommand()
    c.setBuilderName(ManifestIdentifiers.BUILDER_COQ)
    c
  }
}

import org.eclipse.ui.ide.undo.DeleteResourcesOperation

private case class CoqProjectImpl(
    private val res : IProject,
    private val parent : ICoqModel)
    extends ParentImpl(res, parent) with ICoqProject {
  import org.eclipse.ui.ide.undo.CreateProjectOperation
  
  override def getCreateOperation = {
    new CreateProjectOperation(
        ICoqProject.newDescription(res), "New Coq project")
  }
  
  override def getDeleteOperation(deleteContent : Boolean) = {
    new DeleteResourcesOperation(
        Array(res), "Delete Coq project", deleteContent)
  }
  
  import CoqProjectFile._
  import java.io.ByteArrayInputStream
  private def setProjectConfiguration(
      cfg : CoqProjectFile, monitor : IProgressMonitor) = {
    val f = res.getFile("_CoqProject")
    if (!cfg.isEmpty) {
      val contents = new ByteArrayInputStream(
        CoqProjectFile.toString(cfg).getBytes)
      if (f.exists) {
        f.setContents(contents, IResource.NONE, monitor)
      } else f.create(contents, IResource.DERIVED, monitor)
    } else if (f.exists) {
      f.delete(IResource.KEEP_HISTORY, monitor)
    }
  }
  private def getProjectConfiguration : CoqProjectFile = {
    val f = res.getFile("_CoqProject")
    if (f.exists) {
      CoqProjectFile.fromString(
          FunctionIterator.lines(f.getContents).mkString("\n"))
    } else Seq()
  }

  override def getLoadPathProviders : Seq[ICoqLoadPathProvider] = {
    var loadPathMap : Map[String, String] = Map()
    def _util(
        seq : Seq[CoqProjectEntry]) : Seq[ICoqLoadPathProvider] = seq match {
      /* XXX: also parse the -R options later? */
      case (q @ VariableEntry(name, value)) :: tail
          if name.startsWith("KOPITIAM_") =>
        CoqProjectFile.shellTokenise(value) match {
          case "DefaultOutput" :: bindir :: Nil =>
            new DefaultOutputLoadPath(res.getFolder(bindir)) +: _util(tail)
          case "ProjectLoadPath" :: project :: Nil =>
            new ProjectLoadPath(
                res.getWorkspace.getRoot.getProject(project)) +: _util(tail)
          case "SourceLoadPath" :: srcdir :: Nil =>
            new SourceLoadPath(res.getFolder(srcdir)) +: _util(tail)
          case "SourceLoadPath" :: srcdir :: bindir :: Nil =>
            new SourceLoadPath(res.getFolder(srcdir),
                Option(res.getFolder(bindir))) +: _util(tail)
          case "ExternalLoadPath" :: physical :: Nil =>
            new ExternalLoadPath(new Path(physical), None) +: _util(tail)
          case "ExternalLoadPath" :: physical :: logical :: Nil =>
            new ExternalLoadPath(
                new Path(physical), Some(logical)) +: _util(tail)
          case _ => _util(tail)
        }
      case _ :: tail => _util(tail)
      case Nil => Seq.empty
    }
    getProjectConfiguration match {
      case Nil => List(
          new SourceLoadPath(res.getFolder("src")),
          new DefaultOutputLoadPath(getDefaultOutputLocation))
      case pc => _util(pc)
    }
  }
  override def setLoadPathProviders(
      lp : Seq[ICoqLoadPathProvider], monitor : IProgressMonitor) = ()
  
  override def getDefaultOutputLocation = res.getFolder("bin")
  
  override def getPackageFragmentRoot(folder : IPath) =
    new CoqPackageFragmentRootImpl(res.getFolder(folder), this)
  override def getPackageFragmentRoots = getLoadPathProviders.collect {
    case SourceLoadPath(folder, output)
        if (res == folder.getProject) =>
      new CoqPackageFragmentRootImpl(folder, this)
    case DefaultOutputLoadPath(folder)
        if (res == folder.getProject) =>
      new CoqPackageFragmentRootImpl(folder, this)
  }
  
  override def getChildren = getPackageFragmentRoots
}

trait ICoqPackageFragmentRoot extends ICoqElement with IParent {
  override def getCorrespondingResource : Option[IFolder]
  override def getParent : Option[ICoqProject]
  
  override def getElementType = classOf[ICoqPackageFragmentRoot]
  
  def getPackageFragment(folder : IPath) : ICoqPackageFragment
  def getPackageFragments : Seq[ICoqPackageFragment]
  def hasPackageFragments : Boolean = (!getPackageFragments.isEmpty)
  
  def getCreateOperation : IUndoableOperation
  def getDeleteOperation : IUndoableOperation
  
  override def getChildren : Seq[ICoqPackageFragment]
}

private case class CoqPackageFragmentRootImpl(
    private val res : IFolder,
    private val parent : ICoqProject)
    extends ParentImpl(res, parent) with ICoqPackageFragmentRoot {
  import CoqPackageFragmentRootImpl._
  
  import org.eclipse.ui.ide.undo.CreateFolderOperation
  
  override def getCreateOperation = {
    new CreateFolderOperation(res, null, "New package fragment root")
  }
  
  override def getDeleteOperation = {
    new DeleteResourcesOperation(
        Array(res), "Delete package fragment root", false)
  }
  
  private def gpfRecurse(res : IFolder) : List[ICoqPackageFragment] = {
    var results = List[ICoqPackageFragment]()
    if (!res.members.collect(CoqPackageFragmentImpl.fileCollector).isEmpty)
      results = results :+ new CoqPackageFragmentImpl(res, this)
    for (i <- res.members.collect(folderCollector))
      results = results ++ gpfRecurse(i)
    results
  }
  
  override def getPackageFragment(folder : IPath) =
    new CoqPackageFragmentImpl(res.getFolder(folder), this)
  override def getPackageFragments = gpfRecurse(res)
  
  override def getChildren = getPackageFragments
}
private object CoqPackageFragmentRootImpl {
  def folderCollector : PartialFunction[AnyRef, IFolder] = {
    case a : IFolder => a
  }
}

trait ICoqPackageFragment extends ICoqElement with IParent {
  override def getCorrespondingResource : Option[IFolder]
  override def getParent : Option[ICoqPackageFragmentRoot]
  
  override def getElementType = classOf[ICoqPackageFragment]
  
  def getVernacFile(file : IPath) : ICoqVernacFile
  def getVernacFiles : Seq[ICoqVernacFile]
  def hasVernacFiles : Boolean = (!getVernacFiles.isEmpty)
  
  def getObjectFile(file : IPath) : ICoqObjectFile
  def getObjectFiles : Seq[ICoqObjectFile]
  def hasObjectFiles : Boolean = (!getObjectFiles.isEmpty)
  
  def hasCoqFiles : Boolean = (hasVernacFiles || hasObjectFiles)
  
  def getNonCoqFiles : Seq[IFile]
  def hasNonCoqFiles : Boolean = (!getNonCoqFiles.isEmpty)
  
  def getCreateOperation : IUndoableOperation
  def getDeleteOperation : IUndoableOperation
  
  override def getChildren : Seq[ICoqFile]
}

private case class CoqPackageFragmentImpl(
    private val res : IFolder,
    private val parent : ICoqPackageFragmentRoot)
    extends ParentImpl(res, parent) with ICoqPackageFragment {
  import CoqPackageFragmentImpl._
  
  import org.eclipse.ui.ide.undo.CreateFolderOperation
  
  override def getCreateOperation =
    new CreateFolderOperation(res, null, "New package fragment")
  
  override def getDeleteOperation =
    new DeleteResourcesOperation(Array(res), "Delete package fragment", false)
  
  override def getVernacFile(file : IPath) =
    new CoqVernacFileImpl(res.getFile(file), this)
  override def getVernacFiles = {
    res.members.collect(fileCollector).filter(vernacFilter).map(
        new CoqVernacFileImpl(_, this))
  }
  
  override def getObjectFile(file : IPath) =
    new CoqObjectFileImpl(res.getFile(file), this)
  override def getObjectFiles = {
    res.members.collect(fileCollector).filter(objectFilter).map(
        new CoqObjectFileImpl(_, this))
  }
  
  override def getNonCoqFiles = {
    res.members.collect(fileCollector).filterNot(vernacFilter).
        filterNot(objectFilter)
  }
  
  override def getChildren = getVernacFiles ++ getObjectFiles
}
private object CoqPackageFragmentImpl {
  val fileCollector : PartialFunction[AnyRef, IFile] = {
    case a : IFile => a
  }
  
  val vernacFilter = (a : IFile) => {
    a.getContentDescription.getContentType.isKindOf(
        ICoqVernacFile.CONTENT_TYPE)
  }
  
  val objectFilter = (a : IFile) => {
    a.getContentDescription.getContentType.isKindOf(
        ICoqObjectFile.CONTENT_TYPE)
  }
}

trait ICoqFile extends ICoqElement {
  override def getCorrespondingResource : Option[IFile]
  override def getParent : Option[ICoqPackageFragment]
}

import java.io.InputStream

private object EmptyInputStream extends InputStream {
  override def read = -1
}

trait ICoqVernacFile extends ICoqFile {
  override def getElementType = classOf[ICoqVernacFile]
  
  def setContents(is : InputStream, monitor : IProgressMonitor)
  def getCreateOperation : IUndoableOperation
  def getDeleteOperation : IUndoableOperation
}
object ICoqVernacFile {
  import org.eclipse.core.runtime.Platform
  final def CONTENT_TYPE = Platform.getContentTypeManager.getContentType(
      ManifestIdentifiers.CONTENT_TYPE_COQFILE)
}

private case class CoqVernacFileImpl(
    private val res : IFile,
    private val parent : ICoqPackageFragment)
    extends CoqElementImpl(res, parent) with ICoqVernacFile {
  if (!res.getName.endsWith(".v"))
    throw new IllegalArgumentException(res.getName)
  
  import org.eclipse.ui.ide.undo.CreateFileOperation
  
  override def setContents(is : InputStream, monitor : IProgressMonitor) =
    res.setContents(is, IResource.NONE, monitor)
  
  override def getCreateOperation =
    new CreateFileOperation(res, null, EmptyInputStream, "New Vernac file")
  
  override def getDeleteOperation =
    new DeleteResourcesOperation(Array(res), "Delete Vernac file", false)
}

trait ICoqObjectFile extends ICoqFile {
  override def getElementType = classOf[ICoqObjectFile]
  
  def getVernacFile : Option[ICoqVernacFile]
}
object ICoqObjectFile {
  import org.eclipse.core.runtime.Platform
  final def CONTENT_TYPE = Platform.getContentTypeManager.getContentType(
      ManifestIdentifiers.CONTENT_TYPE_COQOBJECTFILE)
}

private case class CoqObjectFileImpl(
    private val res : IFile,
    private val parent : ICoqPackageFragment)
    extends CoqElementImpl(res, parent) with ICoqObjectFile {
  if (!res.getName.endsWith(".vo"))
    throw new IllegalArgumentException(res.getName)
  
  override def getVernacFile = None
}