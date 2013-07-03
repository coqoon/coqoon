/* (c) 2010-2012 Hannes Mehnert and David Christiansen */

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

import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.{SubMonitor, IProgressMonitor}

class CoqCompileJob(source : IFile) extends JobBase(
    "Compiling Coq file " + source.getName, new CoqCompileRunner(source))
class CoqCompileRunner(source : IFile) extends JobRunner[Unit] {
  import org.eclipse.core.runtime.{Path, Status, IStatus}
  import java.io.File
  
  override protected def doOperation(monitor : SubMonitor) : Unit = {
    monitor.beginTask("Compiling " + source, 1)
    
    val location = source.getLocation
    val output = location.removeFileExtension.addFileExtension("vo").toFile
    
    if (output.lastModified > source.getLocation.toFile.lastModified)
      return
    
    if (EclipseConsole.out == null)
      EclipseConsole.initConsole
    val coqc = CoqProgram("coqc")
    //what about dependencies?? <- need Add LoadPath explicitly in .v!
    if (coqc.check) {
      val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
      
      var flp = ICoqModel.forProject(
          source.getProject).getLoadPath.flatMap(_.asArguments)
      if (new File(loadp).exists)
        flp ++= ExternalLoadPath(new Path(loadp), null).asArguments
          
      val cmdarr =
        List(coqc.path, "-noglob") ++ flp ++ List(location.toOSString)
      val coqcp =
        new ProcessBuilder(cmdarr : _*).redirectErrorStream(true).start()
      
      var output = FunctionIterator.lines(coqcp.getInputStream).mkString("\n")
      coqcp.waitFor
      if (coqcp.exitValue != 0)
        fail(new Status(
            IStatus.ERROR, "dk.itu.sdg.kopitiam", output))
      
      monitor.worked(1)
    }
  }
}