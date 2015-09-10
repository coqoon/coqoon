/* CoqLoadPath.scala
 * Coq model objects representing load paths
 * Copyright © 2013, 2014 Alexander Faithfull
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

import dk.itu.coqoon.core.ManifestIdentifiers

class ProjectLoadPathProvider extends LoadPathImplementationProvider {
  import ProjectLoadPathProvider._

  def getImplementation(id : String) : Option[Implementation] =
    if (id.startsWith("project:")) {
      val rest = id.drop("project:".length)
      Some(new Implementation(this, getRoot.getProject(rest)))
    } else None

  def getImplementations() = Nil

  override def getName() = "Project"
}
object ProjectLoadPathProvider {
  import org.eclipse.core.resources.{IProject, ResourcesPlugin}
  private[model] def getRoot() = ResourcesPlugin.getWorkspace.getRoot

  case class Implementation(private val provider : ProjectLoadPathProvider,
      val project : IProject) extends LoadPathImplementation {
    override def getProvider() : LoadPathImplementationProvider = provider

    override def getName() = project.getName
    override def getIdentifier() = makeIdentifier(project)
    
    override def getAuthor() = ""
    override def getDescription() = ""

    import LoadPathImplementation._
    override def getIncompleteLoadPath() =
      if (project.isOpen &&
          project.hasNature(ManifestIdentifiers.NATURE_COQ)) {
        val impls = ICoqModel.toCoqProject(
            project).getLoadPathProviders.flatMap(_.getImplementation)
        val lpes = impls.map(_.getIncompleteLoadPath)
        if (lpes.forall(_.isRight)) {
          Right(lpes.map(_.right.get).flatten)
        } else Left(Broken)
      } else Left(Broken)

    import IncompleteLoadPathEntry.Variable
    override def getValue(name : Variable) : Option[String] = None
  }

  def makeIdentifier(project : IProject) = s"project:${project.getName}"
}

class SourceLoadPathProvider extends LoadPathImplementationProvider {
  import SourceLoadPathProvider._

  def getImplementation(id : String): Option[Implementation] =
    if (id.startsWith("source:")) {
      val rest = id.drop("source:".length)

      import ProjectLoadPathProvider.getRoot
      import dk.itu.coqoon.core.project.CoqProjectFile
      CoqProjectFile.shellTokenise(rest) match {
        case project :: tail =>
          val proj = getRoot.getProject(project)
          tail match {
            case folder :: Nil =>
              Some(new Implementation(this, proj.getFolder(folder), None))
            case folder :: output :: Nil =>
              Some(new Implementation(this, proj.getFolder(folder),
                  Some(proj.getFolder(output))))
            case _ =>
              None
          }
        case _ =>
          None
      }
    } else None

  def getImplementations() = Nil

  override def getName() = "Source"
}
object SourceLoadPathProvider {
  import org.eclipse.core.resources.IFolder

  case class Implementation(
      private val provider : SourceLoadPathProvider, val folder : IFolder,
      val output : Option[IFolder]) extends LoadPathImplementation {
    override def getProvider() : LoadPathImplementationProvider = provider

    override def getName() = folder.getName
    override def getIdentifier() = makeIdentifier(folder, output)

    override def getAuthor() = ""
    override def getDescription() = ""

    import LoadPathImplementation._
    override def getIncompleteLoadPath() =
      Right(Seq(
          IncompleteLoadPathEntry(
              Seq(Right(folder.getLocation.toString)),
              Nil)) ++
          output.map(output => IncompleteLoadPathEntry(
              Seq(Right(output.getLocation.toString)),
              Nil)))

    import IncompleteLoadPathEntry.Variable
    def getValue(v : Variable) = None
  }

  import dk.itu.coqoon.core.project.CoqProjectEntry.escape
  def makeIdentifier(folder : IFolder, output : Option[IFolder]) = {
    val parts = Seq(folder.getProject.getName,
        folder.getProjectRelativePath.toString) ++
        output.map(_.getProjectRelativePath.toString).toSeq
    s"source:" + parts.map(escape).mkString(" ")
  }
}

class DefaultOutputLoadPathProvider extends LoadPathImplementationProvider {
  import DefaultOutputLoadPathProvider._

  def getImplementation(id : String): Option[Implementation] =
    if (id.startsWith("default-output:")) {
      val rest = id.drop("default-output:".length)

      import ProjectLoadPathProvider.getRoot
      import dk.itu.coqoon.core.project.CoqProjectFile
      CoqProjectFile.shellTokenise(rest) match {
        case project :: folder :: Nil =>
          val proj = getRoot.getProject(project)
          Some(new Implementation(this, proj.getFolder(folder)))
        case _ =>
          None
      }
    } else None

  def getImplementations() = Nil

  override def getName() = "Default output"
}
object DefaultOutputLoadPathProvider {
  import org.eclipse.core.resources.IFolder

  case class Implementation(
      private val provider : DefaultOutputLoadPathProvider,
      val folder : IFolder) extends LoadPathImplementation {
    override def getProvider(): DefaultOutputLoadPathProvider = provider

    import dk.itu.coqoon.core.project.CoqProjectEntry.escape
    override def getName() = folder.getName
    override def getIdentifier() = makeIdentifier(folder)

    override def getAuthor() = ""
    override def getDescription() = ""

    import LoadPathImplementation._
    override def getIncompleteLoadPath() =
      Right(Seq(
          IncompleteLoadPathEntry(
              Seq(Right(folder.getLocation.toString)),
              Nil)))

    import IncompleteLoadPathEntry.Variable
    def getValue(v : Variable) = None
  }

  import dk.itu.coqoon.core.project.CoqProjectEntry.escape
  def makeIdentifier(folder : IFolder) = {
    val parts = Seq(folder.getProject.getName,
        folder.getProjectRelativePath.toString)
    s"default-output:" + parts.map(escape).mkString(" ")
  }
}

class ExternalLoadPathProvider extends LoadPathImplementationProvider {
  import ExternalLoadPathProvider._

  def getImplementation(id : String): Option[Implementation] =
    if (id.startsWith("external:")) {
      val rest = id.drop("external:".length)

      import ProjectLoadPathProvider.getRoot
      import dk.itu.coqoon.core.project.CoqProjectFile
      import org.eclipse.core.runtime.Path
      CoqProjectFile.shellTokenise(rest) match {
        case path :: Nil =>
          Some(new Implementation(this, new Path(path), Nil))
        case path :: coqdir :: _ =>
          Some(new Implementation(this, new Path(path), coqdir.split('.')))
        case _ =>
          None
      }
    } else None

  def getImplementations() = Nil

  override def getName() = "External"
}
object ExternalLoadPathProvider {
  import org.eclipse.core.resources.IFolder
  import org.eclipse.core.runtime.IPath
  case class Implementation(private val provider : ExternalLoadPathProvider,
      val fsPath : IPath, val dir : Seq[String])
          extends LoadPathImplementation {
    override def getProvider(): ExternalLoadPathProvider = provider

    import dk.itu.coqoon.core.project.CoqProjectEntry.escape
    override def getName() = fsPath.toString
    override def getIdentifier() = makeIdentifier(fsPath, dir)

    override def getAuthor() = ""
    override def getDescription() = ""

    import LoadPathImplementation._
    override def getIncompleteLoadPath() =
      Right(Seq(
          IncompleteLoadPathEntry(
              Seq(Right(fsPath.toString)),
              dir)))

    import IncompleteLoadPathEntry.Variable
    def getValue(v : Variable) = None
  }

  import dk.itu.coqoon.core.project.CoqProjectEntry.escape
  def makeIdentifier(fsPath : IPath, dir : Seq[String]) = {
    val parts = fsPath.toString +:
      (if (dir == Nil) {
         Nil
       } else Seq(dir.mkString(".")))
    s"external:" + parts.map(escape).mkString(" ")
  }
}

class AbstractLoadPathProvider extends LoadPathImplementationProvider {
  def getImplementation(id : String) =
    if (id.startsWith("abstract:")) {
      val base = id.drop("abstract:".length)
      AbstractLoadPathManager.getInstance.getProviderFor(
          base).flatMap(_.getImplementation(base))
    } else None

  def getImplementations() = Nil

  override def getName() = "Abstract"
}