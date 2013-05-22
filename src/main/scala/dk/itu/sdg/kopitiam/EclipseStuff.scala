/* (c) 2010-2012 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam

trait EclipseUtils {
  //Handy implicits and functions that make dealing with Eclipse less verbose
  import org.eclipse.jface.text.Position
  import org.eclipse.swt.graphics.{Color, RGB}
  import dk.itu.sdg.parsing.{NoLengthPosition, LengthPosition, RegionPosition}

  import UIUtils.getDisplay
  def color (r : Int, g : Int, b : Int) = new Color(getDisplay, r, g, b)

  implicit def pos2eclipsePos (pos : LengthPosition) : Position =
    pos match {
      case NoLengthPosition => new Position(0)
      case RegionPosition(off, len) => new Position(off, len)
    }

  implicit def tuple2Color (vals : (Int, Int, Int)) : Color = color(vals._1, vals._2, vals._3)

  implicit def rgb2color (rgb : RGB) : Color = new Color(getDisplay, rgb)

  def getPrefColor (key : String) : RGB = {
    import org.eclipse.jface.preference.PreferenceConverter
    val store = Activator.getDefault.getPreferenceStore
    PreferenceConverter.getColor(store, key)
  }
}

class CoqJavaProject {
  var ASTdirty : Boolean = false
}

object EclipseBoilerPlate {
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.dialogs.MessageDialog
  def warnUser (title : String, message : String) : Unit = UIUtils.syncExec {
    MessageDialog.openWarning(
        UIUtils.getDisplay.getActiveShell, title, message)
  }
}

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

class CoqCompileJob(source : IFile)
    extends JobBase("Compiling Coq file " + source.getName) {
  override protected def runner = new CoqCompileRunner(source)
}
class CoqCompileRunner(source : IFile) extends SimpleJobRunner {
  import org.eclipse.core.runtime.{IStatus, Status}
  import java.io.File
  
  override protected def doOperation(monitor : SubMonitor) : IStatus = {
    println("CoqCompileJob(" + source + ") is running")
    
    val name = source.getProjectRelativePath.toOSString
    val output = source.getLocation.removeFileExtension.
        addFileExtension("vo").toFile
    val path = source.getProject.getLocation.toFile
    
    if (output.lastModified > source.getLocation.toFile.lastModified)
      return Status.OK_STATUS
    
    if (EclipseConsole.out == null)
      EclipseConsole.initConsole
    val coqc = CoqTopIdeSlave.getProgramPath("coqc")
    //what about dependencies?? <- need Add LoadPath explicitly in .v!
    if (new File(coqc).exists) {
      val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
      val lp = new File(loadp).exists
      val cmdarr =
        if (lp)
          List(coqc, "-noglob", "-R", "src/", "", "-I", loadp, name)
        else
          List(coqc, "-noglob", "-R", "src/", "", name)
      val coqcp = new ProcessBuilder(cmdarr : _*)
            .directory(path)
            .redirectErrorStream(true)
            .start();
      import java.io._
      val ou = new BufferedReader(new InputStreamReader(coqcp.getInputStream()))
      var output = List[String]()
      var line : String = ou.readLine()
      while (line != null) {
        EclipseConsole.out.println(line)
        output :+= line
        line = ou.readLine()
      }
      coqcp.waitFor
      if (coqcp.exitValue != 0)
        return new Status(
            IStatus.ERROR, "dk.itu.sdg.kopitiam", output.mkString("\n"))
      
      monitor.worked(1)
    }
    Status.OK_STATUS
  }
}