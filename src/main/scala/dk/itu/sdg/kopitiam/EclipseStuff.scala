/* (c) 2010-2012 Hannes Mehnert and David Christiansen
 * Copyright © 2013 Alexander Faithfull */

package dk.itu.sdg.kopitiam

object EclipseConsole {
  private val lock = new Object
  import org.eclipse.ui.console.{
    MessageConsole,MessageConsoleStream,ConsolePlugin}
  
  private var out_ : Option[MessageConsoleStream] = None
  
  def out : MessageConsoleStream = lock synchronized {
    out_ match {
      case Some(a) => a
      case None =>
        val console = Some(new MessageConsole("Coq", null))
        ConsolePlugin.getDefault.getConsoleManager.addConsoles(console.toArray)
        out_ = Some(console.get.newMessageStream)
        out_.foreach(_.setEncoding("UTF-8"))
        out_.get
    }
  }
}

import org.eclipse.core.resources.{IFile, IResource}
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.{SubMonitor, IProgressMonitor}

class CoqCompileJob(source : IFile) extends JobBase(
    "Compiling Coq file " + source.getName, new CoqCompileRunner(source, None))
class CoqCompileRunner(
    source : IFile, output : Option[IFile]) extends JobRunner[Unit] {
  import org.eclipse.core.runtime.{Path, IStatus}
  import java.io.{File, FileInputStream}
  
  private var ticker : Option[() => Boolean] = None
  def setTicker(f : Option[() => Boolean]) = (ticker = f)
  
  override protected def doOperation(monitor : SubMonitor) : Unit = {
    monitor.beginTask("Compiling " + source, 2)
    
    val location = source.getLocation.removeFileExtension
    val outputFile = location.addFileExtension("vo").toFile
    
    output match {
      case Some(file)
          if file.getLocalTimeStamp > source.getLocalTimeStamp =>
        return
      case _ =>
    }
    
    val coqc = CoqProgram("coqtop")
    //what about dependencies?? <- need Add LoadPath explicitly in .v!
    if (coqc.check) {
      val flp =
        (ICoqModel.forProject(source.getProject).getLoadPath ++
            Activator.getDefault.getChargeLoadPath).flatMap(_.asArguments)
      val coqcp =
        coqc.run(flp ++ Seq("-noglob", "-compile", location.toOSString),
            a => {
              a.redirectErrorStream(true)
              val process = a.start
              ticker.foreach(w => {
                val thread = new Thread() {
                  setDaemon(true)
                  private def isFinished = try {
                    process.exitValue; true
                  } catch {
                    case e : IllegalThreadStateException => false
                  }
                  override def run = while (!isFinished) {
                    if (!w())
                      process.destroy
                    Thread.sleep(200)
                  }
                }.start
              })
              process
            })
      
      coqcp.readAll match {
        case (i, msgs) if i != 0 =>
          fail(Activator.makeStatus(IStatus.ERROR, msgs))
        case _ =>
      }
      
      monitor.worked(1)
        
      output.foreach(output => {
        val is = new FileInputStream(outputFile)
        if (output.exists) {
          output.setContents(is, IResource.NONE, monitor.newChild(1))
        } else output.create(is, IResource.DERIVED, monitor.newChild(1))
        outputFile.delete
      })
    }
  }
}