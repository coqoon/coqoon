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

class ProjectLoadPathProvider extends AbstractLoadPathProvider {
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
      val project : IProject) extends AbstractLoadPathImplementation {
    override def getProvider() : AbstractLoadPathProvider = provider

    override def getName() = project.getName
    override def getIdentifier() = s"coqoon:project:${getName}"
    
    override def getAuthor() = ""
    override def getDescription() = ""

    import AbstractLoadPathImplementation._
    override def getLoadPath() =
      if (project.isOpen &&
          project.hasNature(ManifestIdentifiers.NATURE_COQ)) {
        Right(ICoqModel.toCoqProject(project).getLoadPath)
      } else Left(Broken)
  }
}

class SourceLoadPathProvider extends AbstractLoadPathProvider {
  import SourceLoadPathProvider._

  def getImplementation(id : String) : Option[Implementation] =
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
      val output : Option[IFolder]) extends AbstractLoadPathImplementation {
    override def getProvider() : AbstractLoadPathProvider = provider

    import dk.itu.coqoon.core.project.CoqProjectEntry.escape
    override def getName() = folder.getName
    override def getIdentifier() = {
      val parts = Seq(folder.getProject.getName,
          folder.getProjectRelativePath.toString) ++
          output.map(_.getProjectRelativePath.toString).toSeq
      s"source:" + parts.map(escape).mkString(" ")
    }

    override def getAuthor() = ""
    override def getDescription() = ""

    import AbstractLoadPathImplementation._
    override def getLoadPath() =
      Right(
          Seq(LoadPathEntry(folder.getLocation, Nil)) ++
          output.map(of => LoadPathEntry(of.getLocation, Nil)))
  }
}

class InterimAbstractLoadPathProvider extends AbstractLoadPathProvider {
  def getImplementation(id : String) =
    if (id.startsWith("abstract:")) {
      val base = id.drop("abstract:".length)
      AbstractLoadPathManager.getInstance.getProviderFor(
          base).flatMap(_.getImplementation(base))
    } else None

  def getImplementations() = Nil

  override def getName() = "Abstract"
}