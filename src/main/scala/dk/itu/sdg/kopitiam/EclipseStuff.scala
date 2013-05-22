/* (c) 2010-2012 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam

trait EclipseUtils {
  //Handy implicits and functions that make dealing with Eclipse less verbose
  import org.eclipse.jface.text.Position
  import org.eclipse.swt.graphics.{Color, RGB}
  import org.eclipse.swt.widgets.Display
  import dk.itu.sdg.parsing.{NoLengthPosition, LengthPosition, RegionPosition}

  private def display = Display.getCurrent
  def color (r : Int, g : Int, b : Int) = new Color(display, r, g, b)

  implicit def pos2eclipsePos (pos : LengthPosition) : Position =
    pos match {
      case NoLengthPosition => new Position(0)
      case RegionPosition(off, len) => new Position(off, len)
    }

  implicit def tuple2Color (vals : (Int, Int, Int)) : Color = color(vals._1, vals._2, vals._3)

  implicit def rgb2color (rgb : RGB) : Color = new Color(display, rgb)

  def getPrefColor (key : String) : RGB = {
    import org.eclipse.jface.preference.PreferenceConverter
    val store = Activator.getDefault.getPreferenceStore
    PreferenceConverter.getColor(store, key)
  }
}

class CoqJavaProject {
  var ASTdirty : Boolean = false

  import org.eclipse.jdt.core.dom.CompilationUnit
  var program : Option[CompilationUnit] = None
}

object JavaPosition {
  /*override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqTheoremDefined(x) =>
        if (editor != null && x.startsWith("valid_")) { // + method.get.id)) {
          //what about specifications!?
          removeMarkers
          val spos = method.get.getStartPosition
          val length = method.get.getLength
          markproven(spos, length)
          //generate proof certificate IF last method!
          val p = getProj
          if (p != null)
            p.program match {
              case None =>
              case Some(prog) =>
                if (proofmarkers.size == countMethods(prog)) {
                  val c = generateCertificate(prog)

                  val nam = p.base_name + "Java.v"

                  val edi = editor.getEditorInput.asInstanceOf[IFileEditorInput]
                  val trfi : IFile = edi.getFile.getProject.getFile(nam)
                  if (trfi.exists)
                    trfi.delete(true, false, null)
                  trfi.create(null, IResource.NONE, null)
                  trfi.setCharset("UTF-8", null)
                  val bytes = c.getBytes("UTF-8")
                  val bs = new Array[Byte](bytes.length)
                  System.arraycopy(bytes, 0, bs, 0, bytes.length)
                  trfi.setContents(new ByteArrayInputStream(bs), IResource.NONE, null)

                  EclipseBoilerPlate.warnUser("Program is correct!", "A proof certificate was generated, its filename is " + nam)
                }
            }
          ActionDisabler.enableStart
        }
      case CoqError(m, n, s, l) =>
        unmark
        if (editor != null) {
          //have coq position, need java position!
          //might either be spec or proof script (or neither of them)
          val proj = getProj
          proj.proofShell match {
            case None =>
              //spec -- or code!
              assert(proj.program != None)
              val pro = proj.program.get
              val spof = pro.getProperty(EclipseJavaASTProperties.specOffset)
              val spoff =
                if (spof != null && spof.isInstanceOf[Int])
                  spof.asInstanceOf[Int]
                else
                  0
              //not sure why 2 here... but otherwise might fail...
              val poff = DocumentState.position + 2 - spoff
              //Console.println("poff is " + poff + ", we're looking at " + DocumentState._content.get.drop(DocumentState.position).take(30) + " specoff points to " + DocumentState._content.get.drop(spoff).take(30))
              //walk over specs, find the one in the right range
              var mym : Option[MethodDeclaration] = None
              var curdist : Int = Integer.MAX_VALUE
              val findM : MethodDeclaration => Unit = x => {
                val off = x.getProperty(EclipseJavaASTProperties.coqOffset)
                assert(off != null)
                assert(off.isInstanceOf[Int])
                val dist = poff - off.asInstanceOf[Int]
                //Console.println("dist here is " + dist + " [" + x.getName.getIdentifier + "]")
                if (dist >= 0 && dist < curdist) {
                  curdist = dist
                  mym = Some(x)
                }
              }
              traverseCU(pro, findM)

              mym match {
                case None => mark(n, -1, 0, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
                case Some(x) =>
                  val doc = getDoc
                  val markOrNot : Any => Boolean = x => {
                    if (x == null || !x.isInstanceOf[Initializer])
                      false
                    else {
                      val xi = x.asInstanceOf[Initializer]
                      val xoff = xi.getProperty(EclipseJavaASTProperties.coqOffset)
                      if (xoff == null || !xoff.isInstanceOf[Int])
                        false
                      else {
                        val mydist = s - xoff.asInstanceOf[Int]
                        Console.println("mydist is " + mydist)
                        if (mydist >= 0) {
                          val offtocolon = doc.get(xi.getStartPosition, xi.getLength).indexOf(":") + 2 //: and ws
                          //Console.println("marking at: [offc] " + offtocolon + " mydist " + mydist)
                          mark(n, xi.getStartPosition + offtocolon + mydist, l, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
                          true
                        } else
                          false
                      }
                    }
                  }

                  if (! (markOrNot(x.getProperty(EclipseJavaASTProperties.postcondition)) ||
                         markOrNot(x.getProperty(EclipseJavaASTProperties.precondition)) ||
                         markOrNot(x.getProperty(EclipseJavaASTProperties.quantification))))
                    mark(n, -1, 0, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
              }
            case Some(x) =>
              //proof!
              next match {
                case Some(st) =>
                  //not entirely correct computation... ("invariant:" and "frame:")
                  val star = s + st.getStartPosition + 3 //"<% "
                  mark(n, star, l, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
                  next = None
                case None =>
                  mark(n, -1, 0, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
              }
          }
        }
    }
  }*/

  import org.eclipse.jdt.core.dom.Statement
  var cur : Option[Statement] = None
  var next : Option[Statement] = None
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