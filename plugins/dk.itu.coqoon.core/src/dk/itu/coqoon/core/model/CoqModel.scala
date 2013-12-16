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
import dk.itu.coqoon.core.coqtop.CoqProgram
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.core.runtime.{IPath, Path, IProgressMonitor}
import org.eclipse.core.resources.{
  IResource, IProjectDescription, ICommand,
  IFile, IFolder, IProject, IWorkspace, IWorkspaceRoot}

trait IParent {
  def getChildren : Seq[ICoqElement]
  def hasChildren : Boolean = (!getChildren.isEmpty)
}

trait ICoqElement {
  def exists : Boolean
  def getAncestor[A]()(implicit a0 : Manifest[A]) : Option[A] =
    this match {
      case q : A => Some(q)
      case _ => getParent.flatMap(_.getAncestor[A])
    }
  def getParent : Option[ICoqElement with IParent]
  def getElementType : Class[_ <: ICoqElement]
  def getCorrespondingResource : Option[IResource]
  def getContainingResource : Option[IResource] =
    getCorrespondingResource.orElse(getParent.flatMap(_.getContainingResource))
  def getModel : ICoqModel = getAncestor[ICoqModel].get

  def accept(f : ICoqElement => Boolean)
}

trait ICoqModel extends ICoqElement with IParent {
  override def getElementType = classOf[ICoqModel]

  override def getParent = None
  override def getCorrespondingResource : Option[IWorkspaceRoot]

  def getProject(name : String) : ICoqProject
  def getProjects : Seq[ICoqProject]
  def hasProjects : Boolean = (!getProjects.isEmpty)

  def toCoqElement(resource : IResource) : Option[ICoqElement]

  def addListener(l : CoqElementChangeListener)
  def removeListener(l : CoqElementChangeListener)
}
object ICoqModel {
  def create(root : IWorkspaceRoot) : ICoqModel = new CoqModelImpl(root)

  private val instance = new CoqModelImpl(
      org.eclipse.core.resources.ResourcesPlugin.getWorkspace.getRoot)
  def getInstance : ICoqModel = instance

  def toCoqProject(project : IProject) : ICoqProject =
    getInstance.toCoqElement(project).flatMap(TryCast[ICoqProject]).orNull
}

trait CoqElementChangeListener {
  def coqElementChanged(ev : CoqElementChangeEvent)
}

abstract class CoqElementChangeEvent(val element : ICoqElement)

case class CoqElementAddedEvent(
    override val element : ICoqElement) extends CoqElementChangeEvent(element)
case class CoqElementRemovedEvent(
    override val element : ICoqElement) extends CoqElementChangeEvent(element)

case class CoqLoadPathChangeEvent(
    override val element : ICoqProject) extends CoqElementChangeEvent(element)

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

sealed trait ICoqLoadPathProvider {
  def getLoadPath() : Seq[CoqLoadPath]
}

case class ProjectLoadPath(
    val project : IProject) extends ICoqLoadPathProvider {
  override def getLoadPath =
    Option(ICoqModel.toCoqProject(project)).map(_.getLoadPath).getOrElse(Seq())
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

case class AbstractLoadPath(
    val identifier : String) extends ICoqLoadPathProvider {
  override def getLoadPath = getProvider.map(_.getLoadPath).getOrElse(Nil)

  def getProvider() =
    AbstractLoadPathManager.getInstance.getProviderFor(identifier)
}

trait AbstractLoadPathProvider {
  def getName() : String
  def getLoadPath() : Seq[CoqLoadPath]
}

class AbstractLoadPathManager {
  private var providers : Map[String, AbstractLoadPathProvider] = Map()

  def getProviderFor(identifier : String) : Option[AbstractLoadPathProvider] =
    providers.get(identifier)

  def setProviderFor(identifier : String, provider : AbstractLoadPathProvider) =
    providers += (identifier -> provider)

  def getProviders() = providers.values.toSeq
}
object AbstractLoadPathManager {
  private final val instance = new AbstractLoadPathManager
  def getInstance() : AbstractLoadPathManager = instance

  import org.eclipse.core.runtime.{CoreException, RegistryFactory}

  for (ice <- RegistryFactory.getRegistry.getConfigurationElementsFor(
           ManifestIdentifiers.EXTENSION_POINT_LOADPATH)
         if ice.getName == "provider") {
    val ex = try {
      TryCast[AbstractLoadPathProvider](
          ice.createExecutableExtension("provider"))
    } catch {
      case e : CoreException => None
    }
    (Option(ice.getAttribute("id")), ex) match {
      case (Some(id), Some(provider)) =>
        getInstance.setProviderFor(id, provider)
      case _ =>
    }
  }
}

class Coq84Library extends AbstractLoadPathProvider {
  override def getName = "Coq 8.4 standard library"

  override def getLoadPath =
      CoqProgram("coqtop").run(Seq("-where")).readAll match {
    case (0, libraryPath_) =>
      val libraryPath = new Path(libraryPath_.trim)
      Seq(CoqLoadPath(libraryPath.append("theories"), Some("Coq")),
          CoqLoadPath(libraryPath.append("plugins"), Some("Coq")),
          CoqLoadPath(libraryPath.append("user-contrib"), None))
    case _ => Nil
  }
}
object Coq84Library {
  final val ID = "dk.itu.sdg.kopitiam/lp/coq/8.4"
}

trait ICoqProject extends ICoqElement with IParent {
  override def getElementType = classOf[ICoqProject]

  override def getParent : Option[ICoqModel]
  override def getCorrespondingResource : Option[IProject]

  def getLoadPath() : Seq[CoqLoadPath]

  def getLoadPathProviders() : Seq[ICoqLoadPathProvider]
  def setLoadPathProviders(
      lp : Seq[ICoqLoadPathProvider], monitor : IProgressMonitor)

  def getDefaultOutputLocation : IFolder

  def getPackageFragmentRoot(folder : IPath) : ICoqPackageFragmentRoot
  def getPackageFragmentRoots : Seq[ICoqPackageFragmentRoot]
  def hasPackageFragmentRoots : Boolean = (!getPackageFragmentRoots.isEmpty)

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

  override def getElementType = classOf[ICoqPackageFragmentRoot]

  def getPackageFragment(folder : IPath) : ICoqPackageFragment
  def getPackageFragments : Seq[ICoqPackageFragment]
  def hasPackageFragments : Boolean = (!getPackageFragments.isEmpty)

  override def getChildren : Seq[ICoqPackageFragment]
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

  override def getChildren : Seq[ICoqFile]
}

trait ICoqFile extends ICoqElement {
  override def getCorrespondingResource : Option[IFile]
  override def getParent : Option[ICoqPackageFragment]
}

import java.io.InputStream

trait ICoqVernacFile extends ICoqFile with IParent {
  override def getElementType = classOf[ICoqVernacFile]

  def setContents(is : InputStream, monitor : IProgressMonitor)

  override def getChildren() : Seq[ICoqScriptElement]
}
object ICoqVernacFile {
  import org.eclipse.core.runtime.Platform
  final def CONTENT_TYPE = Platform.getContentTypeManager.getContentType(
      ManifestIdentifiers.CONTENT_TYPE_COQFILE)
}

sealed trait ICoqScriptElement extends ICoqElement

trait ICoqScriptSentence extends ICoqScriptElement {
  override def getElementType = classOf[ICoqScriptSentence]

  import dk.itu.coqoon.core.utilities.Substring
  def getText() : Substring
  def isSynthetic() : Boolean
}

trait ICoqScriptGroup extends ICoqScriptElement with IParent {
  override def getElementType = classOf[ICoqScriptGroup]

  def getDisposition() : CoqScriptGroupDisposition

  override def getChildren() : Seq[ICoqScriptElement]
}

abstract class CoqScriptGroupDisposition
object NamedCoqGroup {
  def unapply(p : Any) = p match {
    case CoqLtacGroup(name) => Some(name)
    case CoqProofGroup(name) => Some(name)
    case CoqModuleGroup(name) => Some(name)
    case CoqSectionGroup(name) => Some(name)
    case CoqFixpointGroup(name) => Some(name)
    case CoqInductiveGroup(name) => Some(name)
    case CoqDefinitionGroup(name) => Some(name)
    case _ => None
  }
}
case class CoqLtacGroup(val name : String) extends CoqScriptGroupDisposition
case class CoqProofGroup(val name : String) extends CoqScriptGroupDisposition
case class CoqModuleGroup(val name : String) extends CoqScriptGroupDisposition
case class CoqSectionGroup(val name : String) extends CoqScriptGroupDisposition
case class CoqFixpointGroup(
    val name : String) extends CoqScriptGroupDisposition
case class CoqInductiveGroup(
    val name : String) extends CoqScriptGroupDisposition
case class CoqDefinitionGroup(
    val name : String) extends CoqScriptGroupDisposition

trait ICoqObjectFile extends ICoqFile {
  override def getElementType = classOf[ICoqObjectFile]

  def getVernacFile : Option[ICoqVernacFile]
}
object ICoqObjectFile {
  import org.eclipse.core.runtime.Platform
  final def CONTENT_TYPE = Platform.getContentTypeManager.getContentType(
      ManifestIdentifiers.CONTENT_TYPE_COQOBJECTFILE)
}
