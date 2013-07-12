/* (c) 2010-2012 Hannes Mehnert and David Christiansen
 * Copyright © 2013 Alexander Faithfull */

package dk.itu.sdg.kopitiam

object EclipseConsole {
  import org.eclipse.ui.console.{MessageConsole,MessageConsoleStream,IConsole,IConsoleManager,ConsolePlugin}
  var out : MessageConsoleStream = null

  def initConsole () : Unit = {
    val conman : IConsoleManager = ConsolePlugin.getDefault.getConsoleManager
    val existing = conman.getConsoles
    var outputconsole : MessageConsole = null
    if (existing.length > 0) {
      Console.println("have existing console(s) : " + existing.length)
      outputconsole = existing(0).asInstanceOf[MessageConsole]
    } else {
      Console.println("needed to create new console")
      val mycon = new MessageConsole("Coq", null)
      val cons = new Array[IConsole](1)
      cons(0) = mycon
      conman.addConsoles(cons)
      outputconsole = mycon
    }
    out = outputconsole.newMessageStream
    out.setEncoding("UTF-8")
  }

//   display console in workbench!
//   IWorkbenchPage page = ...; obtain the active page
//   String id = IConsoleConstants.ID_CONSOLE_VIEW;
//   IConsoleView view = (IConsoleView) page.showView(id);
//   view.display(myConsole);
}

import org.eclipse.core.resources.{IFile, IResource}
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.{SubMonitor, IProgressMonitor}

class CoqCompileJob(source : IFile) extends JobBase(
    "Compiling Coq file " + source.getName, new CoqCompileRunner(source, None))
class CoqCompileRunner(
    source : IFile, output : Option[IFile]) extends JobRunner[Unit] {
  import org.eclipse.core.runtime.{Path, Status, IStatus}
  import java.io.{File, FileInputStream}
  
  override protected def doOperation(monitor : SubMonitor) : Unit = {
    monitor.beginTask("Compiling " + source, 2)
    
    val location = source.getLocation
    val outputFile = location.removeFileExtension.addFileExtension("vo").toFile
    
    output match {
      case Some(file)
          if file.getLocalTimeStamp > source.getLocalTimeStamp =>
        return
      case _ =>
    }
    
    if (EclipseConsole.out == null)
      EclipseConsole.initConsole
    val coqc = CoqProgram("coqc")
    //what about dependencies?? <- need Add LoadPath explicitly in .v!
    if (coqc.check) {
      val flp =
        (ICoqModel.forProject(source.getProject).getLoadPath ++
            Activator.getDefault.getChargeLoadPath).flatMap(_.asArguments)
      val coqcp = coqc.run("-noglob" +: (flp ++ List(location.toOSString)))
      
      coqcp.readAll match {
        case (i, msgs) if i != 0 =>
          fail(new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", msgs))
        case _ =>
      }
      
      monitor.worked(1)
        
      output.foreach(output => {
        println("Moving file " + outputFile + " to workspace file " + output)
        val is = new FileInputStream(outputFile)
        if (output.exists) {
          output.setContents(is, IResource.NONE, monitor.newChild(1))
        } else output.create(is, IResource.DERIVED, monitor.newChild(1))
        outputFile.delete
      })
    }
  }
}