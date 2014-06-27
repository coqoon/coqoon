/* CoqCompiler.scala
 * Compile Coq proof scripts without polluting the Eclipse workspace
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

package dk.itu.coqoon.core.project

import dk.itu.coqoon.core.ManifestIdentifiers
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.coqtop.CoqProgram
import dk.itu.coqoon.core.utilities.{TryCast, CacheSlot, JobRunner}

import org.eclipse.core.runtime.{Status, IStatus, SubMonitor, IProgressMonitor}
import org.eclipse.core.resources.{IFile, IResource}

class CoqCompilerRunner(source : IFile,
    coqdir : Seq[String]) extends JobRunner[CoqCompilerResult] {
  import java.io.{File, IOException, FileInputStream}

  private var ticker : Option[() => Boolean] = None
  def setTicker(f : Option[() => Boolean]) = (ticker = f)

  private class MonitorThread(
      process : Process, ticker : () => Boolean) extends Thread {
    setDaemon(true)

    private def isFinished = try {
      process.exitValue; true
    } catch {
      case e : IllegalThreadStateException => false
    }

    override def run =
      while (!isFinished) {
        if (!ticker())
          process.destroy
        Thread.sleep(200)
      }
  }

  private def _configureProcess(pb : ProcessBuilder) : Process = {
    pb.redirectErrorStream(true)
    val process = pb.start
    ticker.foreach(t => new MonitorThread(process, t).start)
    process
  }

  private val buffer = CacheSlot[Array[Byte]](new Array[Byte](4096))

  override protected def doOperation(
      monitor : SubMonitor) : CoqCompilerResult = {
    monitor.beginTask("Compiling " + source, 1)

    val location = source.getLocation.removeFileExtension
    val outputFile = location.addFileExtension("vo").toFile

    val coqc = CoqProgram("coqtop")
    if (!coqc.check)
      fail(new Status(IStatus.ERROR,
          ManifestIdentifiers.PLUGIN, "Couldn't find the coqtop program"))

    ICoqModel.getInstance.toCoqElement(source).flatMap(
        TryCast[ICoqVernacFile]).foreach(file => {
      val myPath = file.getCorrespondingResource.get.getFullPath
      val containingRootPath = file.getParent.flatMap(
          _.getParent).flatMap(_.getCorrespondingResource).get.getFullPath
      val qualifiedName = myPath.removeFirstSegments(
          containingRootPath.segmentCount).removeFileExtension.segments
      for (i <- qualifiedName;
           j <- i if j.isWhitespace)
        fail(new Status(IStatus.ERROR, ManifestIdentifiers.PLUGIN,
            "\"" + i + "\" is not a valid part of a Coq qualified name"))
    })

    val owndirArguments =
      Seq("-I", location.removeLastSegments(1).toOSString) ++
          (if (coqdir.length > 0) Seq("-as", coqdir.mkString(".")) else Seq())

    val cp = ICoqModel.toCoqProject(source.getProject)
    val flp = cp.getLoadPath.flatMap(_.asArguments)
    val coqcp = coqc.run(flp ++ owndirArguments ++
        Seq("-noglob", "-compile", location.toOSString), _configureProcess)

    try {
      coqcp.readAll match {
        case (i, msgs) if i != 0 =>
          return CoqCompilerFailure(source, i, msgs)
        case _ =>
      }

      val is = new FileInputStream(outputFile)
      val content = Array.newBuilder[Byte]

      var count = 0
      do {
        content ++= buffer.get.toSeq.take(count)
        count = is.read(buffer.get)
      } while (count != -1)

      CoqCompilerSuccess(source, content.result)
    } catch {
      case e : java.io.IOException => fail(new Status(IStatus.ERROR,
            ManifestIdentifiers.PLUGIN, e.getLocalizedMessage, e))
    } finally outputFile.delete
  }
}

sealed abstract class CoqCompilerResult(val source : IFile)
case class CoqCompilerSuccess(override val source : IFile,
    result : Array[Byte]) extends CoqCompilerResult(source) {
  import java.io.ByteArrayInputStream
  def save(output : IFile, monitor : IProgressMonitor) = {
    val is = new ByteArrayInputStream(result)
    if (output.exists) {
      output.setContents(is, IResource.NONE, monitor)
    } else output.create(is, IResource.NONE, monitor)
  }
}
case class CoqCompilerFailure(override val source : IFile,
    exitCode : Int, messages : String) extends CoqCompilerResult(source)