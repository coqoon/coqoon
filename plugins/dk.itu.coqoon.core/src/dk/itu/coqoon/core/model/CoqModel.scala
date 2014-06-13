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
  def getCorrespondingResource : Option[IResource]
  def getContainingResource : Option[IResource] =
    getCorrespondingResource.orElse(getParent.flatMap(_.getContainingResource))
  def getModel : ICoqModel = getAncestor[ICoqModel].get

  def accept(f : ICoqElement => Boolean)
}

trait ICoqModel extends ICoqElement with IParent {
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
  def create(root : IWorkspaceRoot) : ICoqModel =
    new CoqModelImpl(Option(root))

  private val instance =
    create(org.eclipse.core.resources.ResourcesPlugin.getWorkspace.getRoot)
  def getInstance : ICoqModel = instance

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
case class CoqProjectLoadPathChangedEvent(
    override val element : ICoqProject) extends CoqElementChangedEvent(element)

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
  override def getLoadPath =
    getProvider.map(_.getLoadPath(identifier)).getOrElse(Nil)

  def getProvider() =
    AbstractLoadPathManager.getInstance.getProviderFor(identifier)
}

trait AbstractLoadPathProvider {
  def getName() : String
  def getLoadPath(id : String) : Seq[CoqLoadPath]
}

class AbstractLoadPathManager {
  private var providers : Map[String, AbstractLoadPathProvider] = Map()

  def getProviderFor(identifier : String) : Option[AbstractLoadPathProvider] =
    providers.collectFirst {
      case (prefix, provider) if identifier.startsWith(prefix) => provider
    }

  def addProvider(prefix : String, provider : AbstractLoadPathProvider) =
    providers += (prefix -> provider)

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
    (Option(ice.getAttribute("prefix")), ex) match {
      case (Some(prefix), Some(provider)) =>
        getInstance.addProvider(prefix, provider)
      case _ =>
    }
  }
}

class Coq84Library extends AbstractLoadPathProvider {
  override def getName = "Coq 8.4 standard library"

  override def getLoadPath(id : String) =
    if (Coq84Library.ID == id) {
      CoqProgram("coqtop").run(Seq("-where")).readAll match {
        case (0, libraryPath_) =>
          val libraryPath = new Path(libraryPath_.trim)
          Seq(CoqLoadPath(libraryPath.append("theories"), Some("Coq")),
              CoqLoadPath(libraryPath.append("plugins"), Some("Coq")),
              CoqLoadPath(libraryPath.append("user-contrib"), None))
        case _ => Nil
      }
    } else Nil
}
object Coq84Library {
  final val ID = "dk.itu.sdg.kopitiam/lp/coq/8.4"
}

trait ICoqProject extends ICoqElement with IParent {
  override def getParent : Option[ICoqModel]
  override def getCorrespondingResource : Option[IProject]

  def getLoadPath() : Seq[CoqLoadPath]

  def getLoadPathProviders() : Seq[ICoqLoadPathProvider]
  def setLoadPathProviders(
      lp : Seq[ICoqLoadPathProvider], monitor : IProgressMonitor)

  def getDefaultOutputLocation : Option[IFolder]

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

  def getPackageFragment(folder : IPath) : ICoqPackageFragment
  def getPackageFragments : Seq[ICoqPackageFragment]
  def hasPackageFragments : Boolean = (!getPackageFragments.isEmpty)

  override def getChildren : Seq[ICoqPackageFragment]
}

trait ICoqPackageFragment extends ICoqElement with IParent {
  override def getCorrespondingResource : Option[IFolder]
  override def getParent : Option[ICoqPackageFragmentRoot]

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
  override def getChildren() : Seq[ICoqScriptElement]

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

sealed trait ICoqScriptElement extends ICoqElement {
  def getText() : String
  def getLength() : Int
  def getOffset() : Int
}

trait ICoqScriptSentence extends ICoqScriptElement {
  def isSynthetic() : Boolean
}

trait ICoqLtacSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getIdentifier() = Classifier.LtacSentence.unapply(getText).get._1
  def getBody() = Classifier.LtacSentence.unapply(getText).get._2
}

trait ICoqFixpointSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getKeyword() = Classifier.FixpointSentence.unapply(getText).get._1
  def getIdentifier() = Classifier.FixpointSentence.unapply(getText).get._2
  def getBody() = Classifier.FixpointSentence.unapply(getText).get._3
}

trait ICoqInductiveSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getKeyword() = Classifier.InductiveSentence.unapply(getText).get._1
  def getIdentifier() = Classifier.InductiveSentence.unapply(getText).get._2
  def getBody() = Classifier.InductiveSentence.unapply(getText).get._3
}

trait ICoqDefinitionSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getKeyword() = Classifier.DefinitionSentence.unapply(getText).get._1
  def getIdentifier() = Classifier.DefinitionSentence.unapply(getText).get._2
  def getBinders() = Classifier.DefinitionSentence.unapply(getText).get._3
  def getBody() = Classifier.DefinitionSentence.unapply(getText).get._4
}

trait ICoqLoadSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getIdent() = Classifier.LoadSentence.unapply(getText).get
}

trait ICoqRequireSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getKind() = Classifier.RequireSentence.unapply(getText).get._1
  def getIdent() = Classifier.RequireSentence.unapply(getText).get._2

  def getQualid() : Seq[String] = {
    val ident = getIdent
    if (ident(0) == '"') {
      Seq(ident.substring(1).split("\"", 2)(0))
    } else ident.split("\\s+")
  }
}

trait ICoqAssertionSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getKeyword() = Classifier.AssertionSentence.unapply(getText).get._1
  def getIdentifier() = Classifier.AssertionSentence.unapply(getText).get._2
  def getBody() = Classifier.AssertionSentence.unapply(getText).get._3
}

trait ICoqModuleStartSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getIdentifier() = Classifier.ModuleStartSentence.unapply(getText).get
}

trait ICoqSectionStartSentence extends ICoqScriptSentence {
  import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier
  def getIdentifier() = Classifier.SectionStartSentence.unapply(getText).get
}

trait ICoqScriptGroup extends ICoqScriptElement with IParent {
  override def getText = getChildren.map(_.getText).mkString
  override def getLength = getChildren.foldLeft(0)((a, b) => a + b.getLength)
  override def getOffset = getChildren.head.getOffset

  def getDeterminingSentence() : ICoqScriptSentence =
    getChildren.head.asInstanceOf[ICoqScriptSentence]

  override def getChildren() : Seq[ICoqScriptElement]
}

trait ICoqObjectFile extends ICoqFile {
  def getVernacFile : Option[ICoqVernacFile]
}
object ICoqObjectFile {
  import org.eclipse.core.runtime.Platform
  final def CONTENT_TYPE = Platform.getContentTypeManager.getContentType(
      ManifestIdentifiers.CONTENT_TYPE_COQOBJECTFILE)
}
