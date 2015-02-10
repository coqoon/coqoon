/* CoqBuildScript.scala
 * Utilities for manipulating and updating project build scripts
 * Copyright © 2015 Alexander Faithfull
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

package dk.itu.coqoon.core.project

import dk.itu.coqoon.core.ManifestIdentifiers
import dk.itu.coqoon.core.model.{ICoqProject, AbstractLoadPath}
import org.eclipse.core.runtime.Path
import org.eclipse.core.resources.{IProject, IResource}

object CoqBuildScript {
  import org.eclipse.core.runtime.{CoreException, QualifiedName}
  object WriteBuildScript {
    final private val ID = new QualifiedName(
        ManifestIdentifiers.PLUGIN, "writeBuildScript")

    def get(project : IProject) : Option[Boolean] =
      try {
        Option(project.getPersistentProperty(ID)).map(
            _.equalsIgnoreCase("true"))
      } catch {
        case c : CoreException =>
          None
      }

    def set(project : IProject, b : Option[Boolean]) =
      try {
        project.setPersistentProperty(ID, b.map(_.toString).orNull)
      } catch {
        case c : CoreException =>
          /* do nothing */
      }
  }

  final val currentVersion = 5
  private final val Version = """^_configure_coqoon_version = (\d+)$""".r

  def extractScriptVersion(project : IProject) : Option[Int] = {
    val bsHandle = project.getFile("configure.coqoon.py")
    if (bsHandle.exists && bsHandle.isSynchronized(IResource.DEPTH_ZERO)) {
      import java.io.{BufferedReader, InputStreamReader}
      val r = new BufferedReader(new InputStreamReader(bsHandle.getContents))
      try {
        /* We only bother scanning the first 20-ish lines of the file (the
         * version number should be somewhere around line 14) */
        var count = 0
        var line : Option[String] = None
        do {
          line.foreach(_ match {
            case Version(version) =>
              return Some(Integer.parseInt(version))
            case _ =>
          })
          line = Option(r.readLine)
          count += 1
        } while (line != None && count < 20)
        None
      } finally r.close
    } else None
  }

  def perhapsInstall(project : IProject) : Boolean = {
    val bsHandle = project.getFile("configure.coqoon.py")
    val copyScript =
      if (!bsHandle.exists) {
        /* If the file doesn't exist, then always create it */
        true
      } else if (WriteBuildScript.get(project) == Some(true)){
        /* If the file *does* exist, then only overwrite it if we're supposed
         * to and we have a more recent version */
        extractScriptVersion(project).exists(v => v < currentVersion)
      } else false
    if (copyScript) {
      install(project)
      /* Take responsibility for updating the build script in future */
      WriteBuildScript.set(project, Some(true))
    }
    return copyScript
  }

  def install(project : IProject) = {
    import dk.itu.coqoon.core.Activator
    import org.eclipse.core.runtime.FileLocator
    val bsHandle = project.getFile("configure.coqoon.py")
    val s = FileLocator.find(Activator.getDefault.getBundle,
        new Path("lib/configure.coqoon.py"), null).openStream
    if (bsHandle.exists) {
      bsHandle.setContents(s, 0, null)
    } else bsHandle.create(s, 0, null)

    import dk.itu.coqoon.core.model.ICoqModel
    /* XXX: we should do something when configure.coqoon.vars is outdated */
    generateVars(ICoqModel.toCoqProject(project))
  }

  def generateVars(project : ICoqProject) = {
    import java.io.{BufferedWriter, OutputStreamWriter}
    val vfHandle = project.getCorrespondingResource.map(
        _.getFile("configure.coqoon.vars"))
    vfHandle.foreach(vf => {
      /* We could have use the IncompleteLoadPathEntry.Variable type here, but
       * using a Map automatically cleans up the duplicates for us */
      var vars : Map[String, String] = Map()
      var alpBits : Seq[(String, String, String)] = Seq()
      var alpNames : Map[String, String] = Map()
      project.getLoadPathProviders.foreach(_ match {
        case q @ AbstractLoadPath(id) =>
          q.getImplementation.foreach(impl => {
            alpNames += (id -> impl.getName)
            for (parts <- impl.getIncompleteLoadPath.right;
                 part <- parts) {
              val stringisedPath =
                part.path.foldLeft("")((s, a) => s + (a match {
                  case Left(v) =>
                    vars += (v.name -> v.description)
                    "$(" + v.name + ")"
                  case Right(s) =>
                    s
                }))
              alpBits :+= ((id, part.coqdir.mkString("."), stringisedPath))
            }
          })
        case _ =>
      })
      if (vars.size != 0 && alpBits.length != 0) {
        var document =
"""# Manipulating this project using Coqoon may cause this file to be overwritten
# without warning: any local changes you may have made will not be preserved.

"""
        for ((name, description) <- vars)
          document += s"""var "${name}" "${description}"\n"""
        for ((aid, name) <- alpNames)
          document += s"""alp "${aid}" name "${name}"\n"""
        for ((aid, coqdir, path) <- alpBits)
          document += s"""alp "${aid}" include-recursive "${path}" "${coqdir}"\n"""

        import java.io.ByteArrayInputStream
        val is = new ByteArrayInputStream(document.getBytes("UTF-8"))
        if (vf.exists) {
          vf.setContents(is, 0, null)
        } else vf.create(is, 0, null)
      }
    })
  }
}
