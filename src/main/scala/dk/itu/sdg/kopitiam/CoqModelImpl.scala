/* CoqModelImpl.scala
 * An implementation of the abstraction layer between Eclipse and Coq
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, modify, copy and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.core.runtime._
import org.eclipse.core.resources._

private abstract class CoqElementImpl[
    A <: IResource, B <: ICoqElement with IParent](
    private val res : A, private val parent : B) extends ICoqElement {
  override def getParent = Option(parent)
  override def exists = getCorrespondingResource.exists { _.exists }
  override def getCorrespondingResource = Option(res)

  override def getModel() : CoqModelImpl = getAncestor[CoqModelImpl].get

  protected[kopitiam] def notifyListeners(ev : CoqElementChangeEvent) : Unit =
    getModel.notifyListeners(this, ev)
}

private trait ICache {
  /* Clear the cache, in whole or in part, in response to the changes
   * represented by @ev. */
  def update(ev : IResourceChangeEvent) = destroy
  /* Forget all information stored in the cache. */
  def destroy()
}

class CacheSlot[A](constructor : () => A) {
  private val lock = new Object

  private var slot : Option[A] = None
  def test() = lock synchronized (slot != None)
  def get() = lock synchronized slot match {
    case Some(x) => x
    case None =>
      slot = Option(constructor()); slot.get
  }
  def set(value : Option[A]) = lock synchronized (slot = value)
  def clear() = set(None)
}
object CacheSlot {
  def apply[A](constructor : => A) = new CacheSlot(() => constructor)
}

private abstract class ParentImpl[
    A <: IResource, B <: ICoqElement with IParent](
    private val res : A, private val parent : B)
    extends CoqElementImpl(res, parent) with IParent

private case class CoqModelImpl(
    private val res : IWorkspaceRoot)
    extends ParentImpl(res, null) with ICoqModel {
  import CoqModelImpl._

  import org.eclipse.core.resources
  object WorkspaceListener extends resources.IResourceChangeListener {
    class DeltaVisitor(ev : resources.IResourceChangeEvent)
        extends resources.IResourceDeltaVisitor {
      override def visit(d : resources.IResourceDelta) = {
        toCoqElement(d.getResource).foreach(el => {
          val entry = cache synchronized { cache.get(el) }
          entry.foreach(_.update(ev))
        })
        true
      }
    }

    override def resourceChanged(ev : resources.IResourceChangeEvent) =
        ev.getType() match {
      case resources.IResourceChangeEvent.PRE_BUILD =>
        ev.getDelta().accept(new DeltaVisitor(ev))
      case _ =>
    }
  }
  res.getWorkspace.addResourceChangeListener(
      WorkspaceListener, resources.IResourceChangeEvent.PRE_BUILD)

  override def getProject(name : String) =
    new CoqProjectImpl(res.getProject(name), this)
  override def getProjects = res.getProjects.filter(hasNature).map(
      a => new CoqProjectImpl(a, this))

  override def toCoqElement(resource : IResource) = resource match {
    case p : IProject if hasNature(p) =>
      Some(getProject(p.getName))
    case f : IFolder if hasNature(f.getProject) =>
      None /* XXX: convert into an ICoqPackageFragment(Root) */
    case f : IFile if hasNature(f.getProject) =>
      None /* XXX: convert into an ICoq(Object|Vernac)File */
    case _ => None
  }

  override def getChildren = getProjects

  private var cache = scala.collection.mutable.Map[ICoqElement, ICache]()
  protected[kopitiam] def getCacheFor[A <: ICache](
      element : ICoqElement, constructor : => A)(implicit a0 : Manifest[A]) =
    cache synchronized {
      cache.getOrElseUpdate(element, constructor).asInstanceOf[A]
    }

  private var listeners : Set[CoqElementChangeListener] = Set()

  override def addListener(l : CoqElementChangeListener) = (listeners += l)
  override def removeListener(l : CoqElementChangeListener) = (listeners -= l)

  protected[kopitiam] def notifyListeners(
      source : ICoqElement, ev : CoqElementChangeEvent) : Unit = {
    for (i <- listeners)
      i.coqElementChanged(ev)
  }
}
private object CoqModelImpl {
  def hasNature(a : IProject) =
    (a.isOpen && a.getDescription.getNatureIds.exists(ICoqProject.isCoqNature))
}

import org.eclipse.ui.ide.undo.DeleteResourcesOperation

private case class CoqProjectImpl(
    private val res : IProject,
    private val parent : ICoqModel)
    extends ParentImpl(res, parent) with ICoqProject {
  private class Cache extends ICache {
    def destroy = Seq(projectFile, loadPathProviders, loadPath).map(_.clear)

    override def update(ev : IResourceChangeEvent) = {
      val projectDelta =
        Option(ev.getDelta.findMember(res.getFile("_CoqProject").getFullPath))
      projectDelta match {
        case Some(delta) =>
          /* XXX: Is this a sensible place to send notifications from? */
          notifyListeners(CoqLoadPathChangeEvent(CoqProjectImpl.this))
          destroy
        case None =>
      }
    }

    import CoqProjectFile._
    private[CoqProjectImpl] final val projectFile =
        CacheSlot[CoqProjectFile] {
      val f = res.getFile("_CoqProject")
      if (f.exists) {
        CoqProjectFile.fromString(
            FunctionIterator.lines(f.getContents).mkString("\n"))
      } else Seq()
    }

    private[CoqProjectImpl] final val loadPathProviders =
        CacheSlot[Seq[ICoqLoadPathProvider]] {
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
            case "AbstractLoadPath" :: identifier :: Nil =>
              new AbstractLoadPath(identifier) +: _util(tail)
            case _ => _util(tail)
          }
        case _ :: tail => _util(tail)
        case Nil => Seq.empty
      }
      projectFile.get match {
        case Nil => List(
          new SourceLoadPath(res.getFolder("src")),
          new DefaultOutputLoadPath(res.getFolder("bin")),
          new AbstractLoadPath(AbstractLoadPathManager.COQ_8_4))
        case pc => _util(pc)
      }
    }

    private[CoqProjectImpl] final val loadPath =
        CacheSlot[Seq[CoqLoadPath]] {
      loadPathProviders.get.flatMap(_.getLoadPath)
    }
  }
  private def getCache() = getModel.getCacheFor(this, new Cache)

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
    getCache.projectFile.set(Option(cfg))
  }
  private def getProjectConfiguration : CoqProjectFile =
    getCache.projectFile.get

  override def getLoadPath() = getCache.loadPath.get

  override def setLoadPathProviders(
      lp : Seq[ICoqLoadPathProvider], monitor : IProgressMonitor) = {
    var coqPart : List[CoqProjectEntry] = Nil
    var kopitiamPart : List[CoqProjectEntry] = Nil
    var count = 0
    for (i <- lp) {
      kopitiamPart :+= VariableEntry("KOPITIAM_" + count, (i match {
        case AbstractLoadPath(identifier) =>
          Seq("AbstractLoadPath", identifier)
        case DefaultOutputLoadPath(bin) =>
          val path = bin.getProjectRelativePath.toString
          coqPart :+= RecursiveEntry(path, "")
          Seq("DefaultOutput", bin.getProjectRelativePath.toString)
        case ExternalLoadPath(path_, None) =>
          val path = path_.toString
          coqPart :+= RecursiveEntry(path, "")
          Seq("ExternalLoadPath", path)
        case ExternalLoadPath(path_, Some(coqdir)) =>
          val path = path_.toString
          coqPart :+= RecursiveEntry(path, coqdir)
          Seq("ExternalLoadPath", path, coqdir)
        case ProjectLoadPath(project) =>
          val path = project.getName
          Seq("ProjectLoadPath", project.getName)
        case SourceLoadPath(src, None) =>
          val srcPath = src.getProjectRelativePath.toString
          coqPart :+= RecursiveEntry(srcPath, "")
          Seq("SourceLoadPath", srcPath)
        case SourceLoadPath(src, Some(bin)) =>
          val srcPath = src.getProjectRelativePath.toString
          val binPath = bin.getProjectRelativePath.toString
          coqPart ++= Seq(
            RecursiveEntry(srcPath, ""), RecursiveEntry(binPath, ""))
          Seq("SourceLoadPath", srcPath, binPath)
      }).map(CoqProjectEntry.escape).mkString(" "))
      count += 1
    }
    setProjectConfiguration(coqPart ++ kopitiamPart, monitor)
    notifyListeners(CoqLoadPathChangeEvent(this))
  }
  override def getLoadPathProviders : Seq[ICoqLoadPathProvider] =
    getCache.loadPathProviders.get

  override def getDefaultOutputLocation : IFolder = {
    for (i <- getLoadPathProviders;
         j <- TryCast[DefaultOutputLoadPath](i))
      return j.folder
    res.getFolder("bin")
  }

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

import java.io.InputStream

private object EmptyInputStream extends InputStream {
  override def read = -1
}

private case class CoqVernacFileImpl(
    private val res : IFile,
    private val parent : ICoqPackageFragment)
    extends CoqElementImpl(res, parent) with ICoqVernacFile {
  import java.io.InputStream

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

private case class CoqObjectFileImpl(
    private val res : IFile,
    private val parent : ICoqPackageFragment)
    extends CoqElementImpl(res, parent) with ICoqObjectFile {
  if (!res.getName.endsWith(".vo"))
    throw new IllegalArgumentException(res.getName)

  override def getVernacFile = None
}
