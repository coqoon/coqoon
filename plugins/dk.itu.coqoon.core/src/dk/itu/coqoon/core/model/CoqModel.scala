/* CoqModel.scala
 * An abstraction layer between Eclipse resources and Coq concepts
 * Copyright © 2013 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.core.model

import dk.itu.coqoon.core.{Activator, ManifestIdentifiers}
import dk.itu.coqoon.core.debug.CoqoonDebugPreferences
import dk.itu.coqoon.core.coqtop.CoqProgram
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.core.runtime.{IPath, Path, IProgressMonitor}
import org.eclipse.core.resources.{
  IResource, IProjectDescription, ICommand,
  IFile, IFolder, IProject, IWorkspace, IWorkspaceRoot}

trait IParent {
  def getChildren() : Seq[ICoqElement]
  def hasChildren() : Boolean = (!getChildren.isEmpty)
}

trait ICoqElement {
  def exists() : Boolean
  def getAncestor[A]()(implicit a0 : Manifest[A]) : Option[A] =
    this match {
      case q : A => Some(q)
      case _ => getParent.flatMap(_.getAncestor[A])
    }
  def getParent() : Option[ICoqElement with IParent]
  def getCorrespondingResource() : Option[IResource]
  def getContainingResource() : Option[IResource] =
    getCorrespondingResource.orElse(getParent.flatMap(_.getContainingResource))
  def getModel() : ICoqModel = getAncestor[ICoqModel].get

  import CoqEnforcement.{Issue, Severity}
  def getIssues() : Map[Issue, Severity]
  def setIssues(issues : Map[Issue, Severity])

  def addIssue(issue : (Issue, Severity)) = setIssues(getIssues + issue)
  def addIssues(issues : Map[Issue, Severity]) = setIssues(getIssues ++ issues)

  def accept(f : ICoqElement => Boolean)
}

trait ICoqModel extends ICoqElement with IParent {
  override def getParent = None
  override def getCorrespondingResource : Option[IWorkspaceRoot]

  def getProject(name : String) : ICoqProject
  def getProjects() : Seq[ICoqProject]
  def hasProjects() : Boolean = (!getProjects.isEmpty)

  def toCoqElement(resource : IResource) : Option[ICoqElement]

  def addListener(l : CoqElementChangeListener)
  def removeListener(l : CoqElementChangeListener)
}
object ICoqModel {
  private def create(root : IWorkspaceRoot) : ICoqModel = {
    val impl = new CoqModelImpl(Option(root))
    impl.addListener(IssueTranslator)
    impl
  }

  private lazy val instance =
    create(org.eclipse.core.resources.ResourcesPlugin.getWorkspace.getRoot)
  def getInstance() : ICoqModel = instance

  def toCoqProject(project : IProject) : ICoqProject =
    getInstance.toCoqElement(project).flatMap(TryCast[ICoqProject]).orNull
}

trait CoqElementChangeListener {
  def coqElementChanged(ev : CoqElementEvent)
}

abstract class CoqElementEvent(val element : ICoqElement)

case class CoqElementAddedEvent(
    override val element : ICoqElement) extends CoqElementEvent(element)
case class CoqElementRemovedEvent(
    override val element : ICoqElement) extends CoqElementEvent(element)

abstract class CoqElementChangedEvent(
    override val element : ICoqElement) extends CoqElementEvent(element)
case class CoqFileContentChangedEvent(
    override val element : ICoqFile) extends CoqElementChangedEvent(element)
case class CoqIssuesChangedEvent(
    override val element : ICoqElement) extends CoqElementChangedEvent(element)
case class CoqProjectLoadPathChangedEvent(
    override val element : ICoqProject) extends CoqElementChangedEvent(element)

case class CoqEntitiesChangedEvent(
    override val element : ICoqScriptSentence)
        extends CoqElementChangedEvent(element)

final case class LoadPathEntry(
    path : IPath, coqdir : Seq[String], expandML : Boolean = false) {
  import dk.itu.coqoon.core.CoqoonPreferences

  def asCommands() : Seq[String] = {
    val base =
      (if (CoqoonPreferences.RequireQualification.get) {
        s"""Add LoadPath "${path.toOSString}" """
      } else s"""Add Rec LoadPath "${path.toOSString}" """) +
      (if (coqdir != Nil) {
        " as " + coqdir.mkString(".")
      } else "") + "."
    val ml =
      if (expandML) {
        Seq(s"""Add Rec ML Path "${path.toOSString}".""")
      } else Seq()
    Seq(base) ++ ml
  }

  def asArguments() : Seq[String] = {
    val cd = coqdir.mkString(".")
    val base =
      if (CoqoonPreferences.RequireQualification.get) {
        Seq("-Q", path.toOSString, cd)
      } else Seq("-R", path.toOSString, cd)
    val ml =
      if (expandML) {
        _expand(coqdir, path.toFile) flatMap {
          case (_, path) => Seq("-I", path.toString)
        }
      } else Seq()
    base ++ ml
  }

  import java.io.File

  private def _expand(
      coqdir : Seq[String], f : File) : Seq[(Seq[String], File)] = {
    val l = f.listFiles
    (if (l != null) {
      l.toSeq.filter(_.isDirectory).flatMap(
        f => _expand(coqdir :+ f.getName, f))
    } else Seq.empty) :+ (coqdir, f)
  }

  /* XXX: the RequireQualification code *really* needs to move into the Coq
   * builder! */
  def expand() : Seq[(Seq[String], File)] =
    if (CoqoonPreferences.RequireQualification.get) {
      Seq((coqdir, path.toFile))
    } else {
      val r = _expand(coqdir, path.toFile)
      CoqoonDebugPreferences.LoadPathExpansion.log(
          s"${this} -> ${r}")
      r
    }
}

final case class IncompleteLoadPathEntry(
    path : Seq[Either[IncompleteLoadPathEntry.Variable, String]],
    coqdir : Seq[String], expandML : Boolean = false) {
  import IncompleteLoadPathEntry._
  def complete(p : VariableProvider) :
      Either[IncompleteLoadPathEntry, LoadPathEntry] = {
    val t =
      for (i <- path)
        yield (i match {
          case e @ Left(l) =>
            val v = p.getValue(l)
            if (v != None) {
              Right(v.get)
            } else e
          case e => e
        })
    val v =
      if (t.forall(_.isRight)) {
        Right(LoadPathEntry(
            new Path(t.map(_.right.get).mkString("/")), coqdir, expandML))
      } else Left(IncompleteLoadPathEntry(t, coqdir, expandML))
    CoqoonDebugPreferences.LoadPathResolution.log(s"${this} -> ${v}")
    v
  }
}
object IncompleteLoadPathEntry {
  case class Variable(
      name : String, description : String)
  trait VariableProvider {
    def getValue(v : Variable) : Option[String]
  }

  private final val _beginExpr = """^\$\(""".r
}

case class LoadPathProvider(identifier : String) {
  def getLoadPath =
    getImplementation.flatMap(_.getLoadPath.right.toOption).getOrElse(Nil)

  def getProvider() =
    LoadPathManager.getInstance.getProviderFor(identifier)
  def getImplementation() =
    getProvider.flatMap(_.getImplementation(identifier))
}
object LoadPathProvider {
  def toConfig(p : LoadPathProvider) =
    p match {
      case AbstractLoadPath(identifier) =>
        Seq("AbstractLoadPath", identifier)
      case DefaultOutputLoadPath(bin) =>
        val path = bin.getProjectRelativePath.toString
        Seq("DefaultOutput", bin.getProjectRelativePath.toString)
      case ExternalLoadPath(path_, Nil) =>
        val path = path_.toString
        Seq("ExternalLoadPath", path)
      case ExternalLoadPath(path_, coqdir) =>
        val path = path_.toString
        Seq("ExternalLoadPath", path, coqdir.mkString("."))
      case ProjectLoadPath(project) =>
        val path = project.getName
        Seq("ProjectLoadPath", project.getName)
      case SourceLoadPath(src, None, Seq()) =>
        val srcPath = src.getProjectRelativePath.toString
        Seq("SourceLoadPath", srcPath)
      case SourceLoadPath(src, Some(bin), Seq()) =>
        val srcPath = src.getProjectRelativePath.toString
        val binPath = bin.getProjectRelativePath.toString
        Seq("SourceLoadPath", srcPath, binPath)
      case SourceLoadPath(src, None, coqdir) =>
        val srcPath = src.getProjectRelativePath.toString
        Seq("SourceLoadPathWithCoqdir", srcPath, coqdir.mkString("."))
      case SourceLoadPath(src, Some(bin), coqdir) =>
        val srcPath = src.getProjectRelativePath.toString
        val binPath = bin.getProjectRelativePath.toString
        Seq("SourceLoadPathWithCoqdir",
            srcPath, coqdir.mkString("."), binPath)
    }
}

object ProjectLoadPath {
  import ProjectLoadPathProvider._
  def apply(project : IProject) =
    LoadPathProvider(makeIdentifier(project))
  def unapply(p : LoadPathProvider) =
    p.getImplementation.flatMap(TryCast[Implementation]).map(
        a => a.project)
}

object SourceLoadPath {
  import SourceLoadPathProvider._
  def apply(folder : IFolder, output : Option[IFolder] = None,
      coqdir : Seq[String] = Nil) =
    LoadPathProvider(makeIdentifier(folder, output, coqdir))
  def unapply(p : LoadPathProvider) =
    p.getImplementation.flatMap(TryCast[Implementation]).map(
        a => (a.folder, a.output, a.coqdir))
}

object DefaultOutputLoadPath {
  import DefaultOutputLoadPathProvider._
  def apply(folder : IFolder) =
    LoadPathProvider(makeIdentifier(folder))
  def unapply(p : LoadPathProvider) =
    p.getImplementation.flatMap(TryCast[Implementation]).map(
        a => a.folder)
}

object ExternalLoadPath {
  import ExternalLoadPathProvider._
  def apply(fsPath : IPath, dir : Seq[String]) =
    LoadPathProvider(makeIdentifier(fsPath, dir))
  def unapply(p : LoadPathProvider) =
    p.getImplementation.flatMap(TryCast[Implementation]).map(
        a => (a.fsPath, a.dir))
}

object AbstractLoadPath {
  def apply(id : String) = LoadPathProvider(s"abstract:${id}")
  def unapply(p : LoadPathProvider) =
    if (p.identifier.startsWith("abstract:")) {
      Some(p.identifier.drop("abstract:".length))
    } else None
}

trait LoadPathImplementationProvider {
  def getName() : String

  /* Returns a best-match load path implementation for the given identifier,
   * if there is one.
   *
   * Note that the resulting implementation is not required to work! There are
   * lots of reasons why a provider might not be able to provide a working
   * implementation for a given identifier; see the Status class below. */
  def getImplementation(id : String) : Option[LoadPathImplementation]

  def getImplementations() : Seq[LoadPathImplementation]
}

trait LoadPathImplementation {
  def getProvider() : LoadPathImplementationProvider
  def getIdentifier() : String

  def getName() : String
  def getAuthor() : String
  def getDescription() : String

  import LoadPathImplementation.Excuse
  def getLoadPath() : Either[Excuse, Seq[LoadPathEntry]]
}
object LoadPathImplementation {
  sealed abstract class Excuse

  sealed abstract class Available extends Excuse
  /* Installed but not working */
  final case object Broken extends Available
  /* Installed and (potentially) working, but not compatible with the requested
   * version constraint */
  final case object VersionMismatch extends Available

  sealed abstract class NotAvailable extends Excuse
  /* Not installed, but (potentially) installable */
  final case object Retrievable extends NotAvailable
  /* Not installed and not installable */
  final case object NotRetrievable extends NotAvailable
}

trait IncompleteLoadPathImplementation extends LoadPathImplementation
    with IncompleteLoadPathEntry.VariableProvider {
  import LoadPathImplementation._
  final def getLoadPath() : Either[Excuse, Seq[LoadPathEntry]] =
    getIncompleteLoadPath match {
      case Right(r) =>
        val c = r.map(_.complete(this))
        if (c.forall(_.isRight)) {
          Right(c.map(_.right.get))
        } else Left(Broken)
      case Left(e) => Left(e)
    }

  def getIncompleteLoadPath() : Either[Excuse, Seq[IncompleteLoadPathEntry]]
}

class LoadPathManager {
  private var providers : Seq[LoadPathImplementationProvider] = Seq()
  def getProviders() = providers
  def addProvider(provider : LoadPathImplementationProvider) =
    providers :+= provider

  def getProviderFor(
      identifier : String) : Option[LoadPathImplementationProvider] = {
    for (i <- getProviders;
         j <- i.getImplementation(identifier))
      return Some(i)
    None
  }
}
object LoadPathManager {
  private final val instance = new LoadPathManager
  def getInstance() = instance

  getInstance.addProvider(new ProjectLoadPathProvider)
  getInstance.addProvider(new SourceLoadPathProvider)
  getInstance.addProvider(new DefaultOutputLoadPathProvider)
  getInstance.addProvider(new ExternalLoadPathProvider)
  getInstance.addProvider(new AbstractLoadPathProvider)
}

object AbstractLoadPathManager {
  private final val instance = new LoadPathManager
  def getInstance() = instance

  getInstance.addProvider(new ProvidesLoadPathProvider)

  import org.eclipse.core.runtime.{CoreException, RegistryFactory}

  for (ice <- RegistryFactory.getRegistry.getConfigurationElementsFor(
           ManifestIdentifiers.EXTENSION_POINT_LOADPATH)
         if ice.getName == "provider") {
    val ex = try {
      TryCast[LoadPathImplementationProvider](
          ice.createExecutableExtension("provider"))
    } catch {
      case e : CoreException => None
    }
    ex.foreach(getInstance.addProvider)
  }
}

class CoqStandardLibrary extends LoadPathImplementationProvider {
  override def getName = "Coq standard library"

  override def getImplementation(id : String) =
    if (CoqStandardLibrary.ID == id) {
      Some(new CoqStandardLibrary.Implementation(this, id))
    } else None

  override def getImplementations : Seq[LoadPathImplementation] =
    Seq(new CoqStandardLibrary.Implementation(this))
}
object CoqStandardLibrary {
  import dk.itu.coqoon.core.CoqoonPreferences.CoqPath
  import org.eclipse.jface.util.{PropertyChangeEvent, IPropertyChangeListener}
  private object Listener extends IPropertyChangeListener {
    override def propertyChange(ev : PropertyChangeEvent) =
      if (ev.getProperty == CoqPath.ID)
        path.clear
  }
  Activator.getDefault.getPreferenceStore.addPropertyChangeListener(Listener)
  final val ID = "dk.itu.sdg.kopitiam/lp/coq/8.4"

  import dk.itu.coqoon.core.utilities.CacheSlot
  private val path = CacheSlot {
    CoqProgram.run(Seq("-where")).readAll
  }

  private class Implementation(provider : LoadPathImplementationProvider,
      id : String = ID) extends IncompleteLoadPathImplementation {
    override def getProvider = provider

    override def getIdentifier = id
    override def getName = "Coq standard library"
    override def getAuthor = "Coq development team <coqdev@inria.fr>"
    override def getDescription = "The standard library of Coq."

    import LoadPathImplementation._
    import IncompleteLoadPathEntry.Variable
    override def getIncompleteLoadPath =
      if (id == ID) {
        path.get match {
          case (0, _) =>
            Right(Seq(
                IncompleteLoadPathEntry(
                    Seq(Left(CoqLocation), Right("/theories")),
                    Seq("Coq")),
                IncompleteLoadPathEntry(
                    Seq(Left(CoqLocation), Right("/plugins")),
                    Seq("Coq"), expandML = true),
                IncompleteLoadPathEntry(
                    Seq(Left(CoqLocation), Right("/user-contrib")),
                    Nil)))
          case _ => Left(Broken)
        }
      } else Left(VersionMismatch)

    override def getValue(v : Variable) =
      if (v == CoqLocation) {
        path.get match {
          case (0, path) => Some(path.trim)
          case _ => None
        }
      } else None
  }

  final val CoqLocation = IncompleteLoadPathEntry.Variable(
      "COQ_LOCATION", "Path to the Coq standard library")
}

trait ICoqProject extends ICoqElement with IParent {
  override def getParent() : Option[ICoqModel]
  override def getCorrespondingResource() : Option[IProject]

  /* Note that this method automatically applies any project-defined local
   * overrides for external paths; callers of the getLoadPathProviders method
   * might potentially have to do that themselves. */
  def getLoadPath() : Seq[LoadPathEntry]

  def getLoadPathProviders() : Seq[LoadPathProvider]
  def setLoadPathProviders(
      lp : Seq[LoadPathProvider], monitor : IProgressMonitor)

  def getProvides() : Seq[String]
  def setProvides(ps : Seq[String], monitor : IProgressMonitor)

  def getLocalOverrides() : Map[IPath, IPath]
  def setLocalOverrides(overrides : Map[IPath, IPath])

  def getDefaultOutputLocation() : Option[IFolder]

  def getPackageFragmentRoot(folder : IPath) : ICoqPackageFragmentRoot
  def getPackageFragmentRoots() : Seq[ICoqPackageFragmentRoot]
  def hasPackageFragmentRoots() : Boolean = (!getPackageFragmentRoots.isEmpty)

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

trait ICoqPackageFragmentRoot extends ICoqElement with IParent {
  override def getCorrespondingResource : Option[IFolder]
  override def getParent : Option[ICoqProject]

  def getPackageFragment(folder : IPath) : ICoqPackageFragment
  def getPackageFragments() : Seq[ICoqPackageFragment]
  def hasPackageFragments() : Boolean = (!getPackageFragments.isEmpty)

  override def getChildren : Seq[ICoqPackageFragment]
}

trait ICoqPackageFragment extends ICoqElement with IParent {
  override def getCorrespondingResource : Option[IFolder]
  override def getParent : Option[ICoqPackageFragmentRoot]

  def getCoqdir() : Option[Seq[String]]

  def getVernacFile(file : IPath) : ICoqVernacFile
  def getVernacFiles() : Seq[ICoqVernacFile]
  def hasVernacFiles() : Boolean = (!getVernacFiles.isEmpty)

  def getObjectFile(file : IPath) : ICoqObjectFile
  def getObjectFiles() : Seq[ICoqObjectFile]
  def hasObjectFiles() : Boolean = (!getObjectFiles.isEmpty)

  def hasCoqFiles() : Boolean = (hasVernacFiles || hasObjectFiles)

  def getNonCoqFiles() : Seq[IFile]
  def hasNonCoqFiles() : Boolean = (!getNonCoqFiles.isEmpty)

  override def getChildren : Seq[ICoqFile]
}

trait ICoqFile extends ICoqElement {
  override def getCorrespondingResource : Option[IFile]
  override def getParent : Option[ICoqPackageFragment]
}

import java.io.InputStream

trait ICoqVernacFile extends ICoqFile with IParent {
  override def getChildren() : Seq[ICoqScriptElement]

  def getObjectFile() : Option[ICoqObjectFile]

  def getLineOffset(line : Int) : Option[Int]
  def getSentenceAt(offset : Int) : Option[ICoqScriptSentence]

  def detach() : IDetachedCoqVernacFile
}
object ICoqVernacFile {
  import org.eclipse.core.runtime.Platform
  final def CONTENT_TYPE = Platform.getContentTypeManager.getContentType(
      ManifestIdentifiers.CONTENT_TYPE_COQFILE)
}

trait IDetachedCoqVernacFile extends ICoqVernacFile {
  def commit(monitor : IProgressMonitor)

  def getContents() : String
  def setContents(contents : String)
}
object IDetachedCoqVernacFile {
  def createDummy() : IDetachedCoqVernacFile = new DetachedCoqVernacFileImpl(
      new CoqVernacFileImpl(None, new CoqPackageFragmentImpl(
          None, new CoqPackageFragmentRootImpl(
              None, new CoqProjectImpl(None,
                  new CoqModelImpl(None))))))
}

sealed trait ICoqScriptElement extends ICoqElement {
  def getText() : String
  def getLength() : Int
  def getOffset() : Int
}

trait ICoqScriptSentence extends ICoqScriptElement {
  def isSynthetic() : Boolean

  def getEntities() : Map[(Int, Int), ICoqEntity]
  def setEntities(entities : Map[(Int, Int), ICoqEntity])
  def addEntity(position : Int, length : Int, entity : ICoqEntity) =
    setEntities(getEntities + ((position, length) -> entity))
}

trait ICoqEntity {
  def open() : Unit
}

trait ICoqScriptGroup extends ICoqScriptElement with IParent {
  override lazy val getText = getChildren.map(_.getText).mkString
  override lazy val getLength =
    getChildren.foldLeft(0)((a, b) => a + b.getLength)
  override lazy val getOffset = getChildren.head.getOffset

  def getDeterminingSentence() : ICoqScriptSentence =
    getChildren.head.asInstanceOf[ICoqScriptSentence]

  override def getChildren() : Seq[ICoqScriptElement]
}

trait ICoqObjectFile extends ICoqFile {
  /* Returns the (actually extant) source files that could have produced this
   * object file. (Having more than one file in this sequence should normally
   * be treated as an error.) */
  def getVernacFiles() : Seq[ICoqVernacFile]

  def isQuick() : Boolean
}
object ICoqObjectFile {
  import org.eclipse.core.runtime.Platform
  final def CONTENT_TYPE = Platform.getContentTypeManager.getContentType(
      ManifestIdentifiers.CONTENT_TYPE_COQOBJECTFILE)
}
