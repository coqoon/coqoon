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

class ProvidesLoadPathProvider extends LoadPathImplementationProvider {
  import dk.itu.coqoon.core.model.{ICoqModel, ICoqProject}

  case class Implementation(val project : ICoqProject,
      val identifier : String) extends LoadPathImplementation {
    override def getProvider = ProvidesLoadPathProvider.this
    override def getName() = {
      val r = project.getCorrespondingResource.map(
          "project " + _.getName).getOrElse("(mystery project)")
      s"$identifier (provided by $r)"
    }
    override def getIdentifier() = identifier
    override def getAuthor() = ""
    override def getDescription() = ""
    override def getLoadPath = Right(project.getLoadPath)
  }

  override def getImplementation(
      id : String) : Option[LoadPathImplementation] = {
    for (p <- ICoqModel.getInstance.getProjects;
         provides <- p.getProvides if provides == id) {
      return Some(new Implementation(p, provides))
    }
    return None
  }
  override def getImplementations() = Nil

  override def getName() = "(Provides)"
}

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
    override def getLoadPath() =
      if (project.isOpen &&
          project.hasNature(ManifestIdentifiers.NATURE_COQ)) {
        Right(ICoqModel.toCoqProject(project).getLoadPath)
      } else Left(Broken)
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
        case project +: tail =>
          val proj = getRoot.getProject(project)
          tail match {
            case folder +: "" +: coqdir +: Seq()  =>
              Some(new Implementation(this,
                  proj.getFolder(folder), None, coqdir.split("\\.").toSeq))
            case folder +: output +: coqdir +: Seq() =>
              Some(new Implementation(this, proj.getFolder(folder),
                  Some(proj.getFolder(output)), coqdir.split("\\.").toSeq))
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
      val output : Option[IFolder], val coqdir : Seq[String])
      extends LoadPathImplementation {
    override def getProvider() : LoadPathImplementationProvider = provider

    override def getName() = folder.getName
    override def getIdentifier() = makeIdentifier(folder, output, coqdir)

    override def getAuthor() = ""
    override def getDescription() = ""

    override def getLoadPath() =
      Right(Seq(
          LoadPathEntry(folder.getLocation, coqdir)) ++
          output.map(output => LoadPathEntry(output.getLocation, coqdir)))
  }

  import dk.itu.coqoon.core.project.CoqProjectEntry.escape
  def makeIdentifier(
      folder : IFolder, output : Option[IFolder], coqdir : Seq[String]) = {
    val parts = Seq(folder.getProject.getName,
        folder.getProjectRelativePath.toString,
        output.map(_.getProjectRelativePath.toString).getOrElse(""),
        coqdir.mkString("."))
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

    override def getLoadPath() =
      Right(Seq(LoadPathEntry(folder.getLocation, Nil)))
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

    override def getLoadPath() =
      Right(Seq(LoadPathEntry(fsPath, dir)))
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