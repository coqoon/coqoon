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

class CoqJavaProject (basename : String) {
  def base_name : String = basename

  import org.eclipse.jface.text.IDocument
  var ASTdirty : Boolean = false
  var proofShell : Option[CoqShellTokens] = None

  import org.eclipse.jdt.core.dom.CompilationUnit
  var program : Option[CompilationUnit] = None

  def isJava (doc : IDocument) : Boolean = false
  def isCoqModel (doc : IDocument) : Boolean = false
}

object JavaPosition extends EclipseJavaHelper {
  import JavaASTUtils._
  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  var editor : JavaEditor = null

  import org.eclipse.jdt.core.dom.MethodDeclaration
  var method : Option[MethodDeclaration] = None

  import org.eclipse.jface.text.IDocument
  def getDoc () : IDocument = {
    if (editor != null) {
      val prov = editor.getDocumentProvider
      prov.getDocument(editor.getEditorInput)
    } else null
  }

  def getProj() : CoqJavaProject = null

  import org.eclipse.jdt.core.dom.CompilationUnit
  def generateCertificate (c : CompilationUnit) : String = {
    //prog
    val pdef = EclipseJavaASTProperties.getDefinition(c).get.mkString("\n")
    //spec
    val spec = EclipseJavaASTProperties.getSpecification(c).get.mkString("\n")
    //prog_valid

    //proofs
    val ps = traverseCU(c, proofScript)
    //prog_valid_proof

    //end
    val en = c.getProperty(EclipseJavaASTProperties.coqEnd).asInstanceOf[String]
    pdef + spec + ps.mkString("\n") + "\n" + en
  }

  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.{IFile, IMarker, IResource}
  import java.io.ByteArrayInputStream
  import org.eclipse.jdt.core.dom.Initializer
  /*override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqProofCompleted() =>
        if (editor != null) {
          CoqCommands.doLater(() => {
            DocumentState.setBusy
            CoqTop.writeToCoq("Qed.")
          })
          //if for some reason the Shell came before the proof completed
          if (DocumentState.readyForInput)
            CoqCommands.step
        }
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
      case CoqShellReady(monoton, tokens) =>
        if (monoton)
          reAnnotate(false, false)
        else
          reAnnotate(true, true)
      case _ =>
    }
  }*/

  def retract () : Unit = {
    val mn = (method != None)
    Console.println("retracting with " + editor + " and method? " + mn)
    if (editor != null && method != None) {
      Console.println("hello my friend, NONONONO")
      emptyCoqShells
      method = None
      if (getProj != null)
        getProj.proofShell = None
    }
  }

  import org.eclipse.jdt.core.dom.Statement
  var cur : Option[Statement] = None
  var next : Option[Statement] = None

  import org.eclipse.jdt.core.dom.{EmptyStatement, WhileStatement, IfStatement, Block}
  import scala.collection.immutable.Stack

  def proofScript (m : MethodDeclaration) : String = {
    val prov = editor.getDocumentProvider
    val ei = editor.getEditorInput
    val doc = prov.getDocument(ei)
    val prf = m.getProperty(EclipseJavaASTProperties.coqProof)
    assert(prf != null)
    var res : String = prf.asInstanceOf[String]

    val printer : Statement => Option[String] = x => printProofScript(doc, x)
    val rs = traverseAST(m, true, false, printer)
    res + rs.mkString("\n") + "\nQed."
  }

  def emptyCoqShells () : Unit = (/* do nothing */)

  def getASTbeforeOff (off : Int) : Option[Statement] = (/* do nothing */ None)

  def getLastCoqStatement () : Option[CoqShellTokens] = (/* do nothing */ None)

  def copyProps (from : MethodDeclaration, to : MethodDeclaration) = (/* do nothing */)

  import org.eclipse.jface.text.ITextSelection
  def getCoqCommand () : Option[String] = {
    val prov = editor.getDocumentProvider
    val ei = editor.getEditorInput
    val doc = prov.getDocument(ei)
    //hold on if ASTdirty or modelNewerThanSource!
    assert(next == None)
    if (getProj.ASTdirty) {
      //good news: we do not need to backtrack (already done)
      //bad news: we need to update our internal representation
      // and copy over properties (at least CoqShell)
      val bla = getRoot(ei)
      val cu = getCompilationUnit(bla)
      walkAST(null, cu, doc)

      val selection = editor.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
      val off = selection.getOffset
      val node = findASTNode(cu, off, 0)
      findMethod(node) match {
        case None =>
        case Some(md) =>
          //copy over
          copyProps(method.get, md)
          getProj.program = Some(cu)
          method = Some(md)
          getProj.ASTdirty = false
      }
    }

    var active : Boolean = (cur == None)

    val print : Statement => Option[String] = x =>
      if (active) {
        val ps = printProofScript(doc, x)
        ps match {
          case None => None
          case Some(ps) =>
            next = Some(x)
            Some(ps)
        }
      } else {
        if (cur.get == x)
          active = true
        None
      }

    val r = traverseAST(method.get, true, true, print)
    assert(r.size == 1 || r.size == 0)
    if (r.size == 1)
      Some(r.head)
    else
      None
  }

  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.text.source.IAnnotationModelExtension
  def reAnnotate (proc : Boolean, undo : Boolean) : Unit = {
    //4 cases:
    // #t #f => processing       remove nothing, mark yellow
    // #t #t => problem marker   remove yellow, mark nothing
    // #f #t => real undo        remove green, mark green
    // #f #f => processed!       remove yellow & green, mark green
    if (editor != null && method != None) {
      Console.println("reannotate called with proc " + proc + " undo " + undo + " method " + (method != None) + " editor " + editor)
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val proj = getProj
      if (proj.proofShell != None) {
        //we're in proof mode!

        val start = method.get.getStartPosition

        if (!proc && undo) {
          cur = next
          next = None
        }

        Console.println(" reAnn (cur: " + cur + " next: " + next + ") proc " + proc + " undo " + undo)
        if (next != None && !proc && !undo) {
          Console.println("  ass " + cur + " now " + next)
          cur = next
          next = None
        }
      }
/*      if (proj.modelShell != None) {
        val coqoff = (if (proc) DocumentState.sendlen else 0) + DocumentState.position //- proj.program.get.getSpecOffset - method.get.getSpecOff - method.get.getSpecLength - 1
        Console.println("spec? coqoff here is now " + coqoff)
        val annmodel = prov.getAnnotationModel(editor.getEditorInput)
        annmodel.connect(doc)
        if ((proc && undo) || (!proc && !undo)) {
          specprocessing match {
            case Some(x) => annmodel.removeAnnotation(x)
            case None =>
          }
          specprocessing = None
        }

        if (coqoff == 0 && !(proc && undo)) {
          //search in method specs for min/max
          //mark these!
          var start : Int = Int.MaxValue
          var end : Int = 0
/*          for (s <- method.get.getSpecs) {
            if (s.pos.line < start)
              start = s.pos.line
            if (s.pos.line > end)
              end = s.pos.line
          }
*/
          val txt =
            if (proc)
              "dk.itu.sdg.kopitiam.processing"
            else
              "dk.itu.sdg.kopitiam.processed"
          val soff = doc.getLineOffset(start - 1)
          val eoff = doc.getLineOffset(end) - 1
          Console.println("spec coloring " + txt + " annotation for lines: " + start + " -- " + end)
          val sma = new Annotation(txt, false, "Proof")
          annmodel.addAnnotation(sma, new Position(soff, eoff - soff))
          if (proc)
            specprocessing = Some(sma)
          else
            specprocessed ::= sma
        }
        annmodel.disconnect(doc)
      } */
    }
  }
}

object EclipseBoilerPlate {
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.dialogs.MessageDialog
  def warnUser (title : String, message : String) : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = MessageDialog.openWarning(Display.getDefault.getActiveShell, title, message)
      })
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

class CoqCompileJob (source : IFile)
    extends Job("Compiling Coq file " + source.getName) {
  override def run(monitor : IProgressMonitor) =
    CoqCompileJob.run(source, monitor)
}
object CoqCompileJob {
  import org.eclipse.core.runtime.{IStatus, Status}
  import java.io.File
  
  def run(source : IFile, monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(
        monitor_, "Compiling " + source.getName, 1)
    try {
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
        println(cmdarr)
        val coqcp = new ProcessBuilder(cmdarr : _*)
              .directory(path)
              .redirectErrorStream(true)
              .start();
        import java.io._
        val ou = new BufferedReader(new InputStreamReader(coqcp.getInputStream()))
        var line : String = ou.readLine()
        while (line != null) {
          EclipseConsole.out.println(line)
          line = ou.readLine()
        }
        coqcp.waitFor
        if (coqcp.exitValue != 0)
          return new Status(IStatus.ERROR,
              "dk.itu.sdg.kopitiam", "Oh no! Last output was " + line)
        
        monitor.worked(1)
      }
      Status.OK_STATUS
    } finally monitor_.done
  }
}