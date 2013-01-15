/* (c) 2010-2012 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam

trait EclipseUtils {
  //Handy implicits and functions that make dealing with Eclipse less verbose
  import org.eclipse.jface.text.Position
  import org.eclipse.swt.graphics.{Color, RGB}
  import org.eclipse.swt.widgets.Display
  import dk.itu.sdg.parsing._

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
  //foo -> foo.java [Java], foo.v [Model] ~> foo.java.v [Complete]
  import scala.collection.immutable.HashMap
  import org.eclipse.jface.text.IDocument

  var javaSource : Option[IDocument] = None
  var coqModel : Option[IDocument] = None
  var coqSource : Option[IDocument] = None
  var modelNewerThanSource : Boolean = true
  var javaNewerThanSource : Boolean = true
  var modelShell : Option[CoqShellTokens] = None
  var proofShell : Option[CoqShellTokens] = None

  import org.eclipse.jdt.core.dom.CompilationUnit
  var program : Option[CompilationUnit] = None

  def gotClosed (doc : IDocument) : Unit = {
    javaSource match {
      case Some(d) => if (d == doc) javaSource = None
      case _ =>
    }
    coqModel match {
      case Some(d) => if (d == doc) coqModel = None
      case _ =>
    }
    coqSource match {
      case Some(d) => if (d == doc) coqSource = None
      case _ =>
    }
  }

  def setDocument (doc : IDocument, name : String) : Unit = {
    if (name.equals(basename + ".java")) {
      assert(javaSource == None)
      javaSource = Some(doc)
    } else if (name.equals(basename + ".v")) {
      assert(coqModel == None)
      coqModel = Some(doc)
    } else if (name.equals(basename + ".java.v")) {
      assert(coqSource == None)
      coqSource = Some(doc)
    } else
      Console.println("huh? " + name)
  }

  private def optEq (x : Option[IDocument], y : IDocument) : Boolean = {
    x match {
      case Some(x2) => x2 == y
      case _ => false
    }
  }

  def isJava (doc : IDocument) : Boolean = { optEq(javaSource, doc) }
  def isCoqModel (doc : IDocument) : Boolean = { optEq(coqModel, doc) }
  def isCoqSource (doc : IDocument) : Boolean = { optEq(coqSource, doc) }

  import org.eclipse.ui.{IFileEditorInput, PlatformUI}
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.core.resources.{IFile, IMarker, IProject}
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jdt.core.dom.ASTNode
  def proveMethod (n : ASTNode) : Unit = {
    modelShell match {
      case Some(x) =>
        if (x.globalStep > CoqState.getShell.globalStep)
          modelShell = None
      case None =>
    }
    proofShell match {
      case Some(x) =>
        if (x.globalStep > CoqState.getShell.globalStep)
          proofShell = None
      case None =>
    }
    Console.println("provemethod called! modelnewer: " + modelNewerThanSource + " javanewer: " + javaNewerThanSource + " modelshell " + modelShell + " proofshell " + proofShell)
    if (modelNewerThanSource || modelShell == None) {
      modelNewerThanSource = false
      var open : Boolean = false
      var model : IFile = null
      coqModel match {
        case None => //need to open editor and run
          if (JavaPosition.editor == null)
            Console.println("this should not happen - no coqmodel and no java editor...")
          else {
            val fei = JavaPosition.editor.getEditorInput
            if (fei.isInstanceOf[IFileEditorInput]) {
              val proj : IProject = fei.asInstanceOf[IFileEditorInput].getFile.getProject
              val maybemodel = proj.getFile(basename + ".v")
              if (maybemodel.exists)
                model = maybemodel
              else {
                val maybemodel = proj.getFile("src/" + basename + ".v")
                if (maybemodel.exists)
                  model = maybemodel
              }
            } else
              JavaPosition.mark("something went wrong reading the Java file", 0, 10, IMarker.PROBLEM, IMarker.SEVERITY_WARNING)
            if (model == null || ! model.exists)
              JavaPosition.mark("Please write a model file for this java file with a Module named '" + basename + "_model'.", 0, 10, IMarker.PROBLEM, IMarker.SEVERITY_WARNING)
            else
              open = true
          }
        case Some(x) =>
      }
      CoqCommands.doLater(() => {
        Console.println("activating model editor for " + basename + " with open " + open)
        DocumentState._content = None
        Display.getDefault.syncExec(
          new Runnable() {
            def run() = {
              val wbp = PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage
              if (open)
                wbp.openEditor(new FileEditorInput(model), "kopitiam.CoqEditor")
              else
                for (y <- wbp.getEditorReferences) {
                  val z = y.getEditorInput
                  if (z.isInstanceOf[IFileEditorInput])
                    if (z.asInstanceOf[IFileEditorInput].getFile.getName.equals(basename + ".v"))
                      wbp.activate(y.getEditor(true))
                }
            }})
        Console.println("stepping over model! ")
        CoqStepAllAction.doitH
      })
      CoqCommands.doLater(() => {
        modelShell = Some(CoqState.getShell)
        DocumentState.resetState
        Console.println("preserving checkpoint " + modelShell)
        CoqCommands.step
      })
      if (! javaNewerThanSource) {
        CoqCommands.doLater(() => {
          Console.println("now back to java editor")
          Display.getDefault.syncExec(
            new Runnable() {
              def run() = {
                PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage.activate(JavaPosition.editor)
              }})
          CoqCommands.step
        })
      }
    }
    if (javaNewerThanSource) {
      javaNewerThanSource = false
      //safety first
      CoqCommands.doLater(() => {
        Console.println("activating javaeditor")
        Display.getDefault.syncExec(
          new Runnable() {
            def run() = {
              PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage.activate(JavaPosition.editor)
              //TODO: shouldn't work on a file-basis here anyways
              if (JavaPosition.editor.isDirty)
                JavaPosition.editor.doSave(null)
            }})
        //retract up until model
        modelShell match {
          case None => Console.println("how did I get here?")
          case Some(x) =>
            DocumentState.resetState
            JavaPosition.retract
            if (x.globalStep < CoqState.getShell.globalStep) {
              DocumentState.setBusy
              Console.println("backtracking to " + x)
              CoqTop.writeToCoq("Backtrack " + x.globalStep + " 0 " + CoqState.getShell.context.length + ".")
            } else
              CoqCommands.step
        }
      })
    }
    CoqCommands.doLater(() => {
      if (DocumentState.activeEditor != null) {
        DocumentState.activeEditor.addAnnotations(0, 0)
        DocumentState.activeEditor.invalidate
        DocumentState.activeEditor = null
      }
      CoqCommands.step
    })
    CoqCommands.doLater(() => {
      proofShell match {
        case None =>
          Console.println("sending defs + spec")
          DocumentState._content = getCoqString
          PrintActor.register(JavaPosition)
          CoqStepAllAction.doit
        case Some(x) =>
/*          if (x.globalStep < CoqState.getShell.globalStep)
            if (JavaPosition.method != meth) {
              DocumentState.setBusy
              Console.println("backtracking to proofshell " + x)
              CoqTop.writeToCoq("Backtrack " + x.globalStep + " 0 " + CoqState.getShell.context.length + ".")
              JavaPosition.method = None
              DocumentState.position = getCoqString.get.length
              JavaPosition.method = meth
            } else
              CoqCommands.step
          } else {
            JavaPosition.method = meth
            CoqCommands.step
          } */
      }
    })
    CoqCommands.doLater(() => {
      if (proofShell == None) {
        Console.println("preserving proof shell: " + CoqState.getShell)
        proofShell = Some(CoqState.getShell)
      }
      //story so far: model is now updated, java might be newly generated!
      //all has been sent! - hasn't it?
      JavaPosition.method match {
        case Some(x) =>
          val prf = x.getProperty(EclipseJavaASTProperties.coqProof)
          assert(prf != null)
          val p = prf.asInstanceOf[String]
          JavaPosition.proofoffset = DocumentState.position
          val off = DocumentState.position + p.size + 2 //why 2 here?
          //locate index of given ASTNode and go there!
          DocumentState._content = Some(DocumentState._content.get + p)
          PrintActor.register(JavaPosition)
          if (DocumentState.position < off)
            CoqStepUntilAction.reallydoit(off)
        case None =>
          CoqCommands.step
      }
    })
  }

  def getCoqString () : Option[String] = {
    program match {
      case None =>
        Console.println("no program!")
        None
      case Some(x) =>
        val pdef = x.getProperty(EclipseJavaASTProperties.coqDefinition).asInstanceOf[String]
        val spec = x.getProperty(EclipseJavaASTProperties.coqSpecification).asInstanceOf[String]
        val res = pdef + spec
        Console.println("getcoqstring returns " + res)
        Some(res)
    }
  }
}

object EclipseTables {
  import scala.collection.mutable.HashMap
  import org.eclipse.jface.text.IDocument

  val DocToProject = new HashMap[IDocument, CoqJavaProject]()
  val StringToProject = new HashMap[String, CoqJavaProject]()
}

/*
import akka.actor._
class MyTimer extends Actor {
  def receive = {
    case ("START", x : Int) =>
      Thread.sleep(1000);
      if (CoqProgressMonitor.tick == x) {
        //Console.println("tick " + CoqProgressMonitor.tick + " is equal to x " + x)
        CoqProgressMonitor.actor.tell("REALLY")
      }
  }
}

object CoqProgressMonitor {
  var tick : Int = 0
  var timer : ActorRef = null
  var actor : ActorRef = null
  var multistep : Boolean = false
}

class CoqProgressMonitorImplementation extends Actor {
  import org.eclipse.core.runtime.IProgressMonitor
  import org.eclipse.jface.dialogs.ProgressMonitorDialog
  import org.eclipse.swt.widgets.Shell
  import org.eclipse.swt.widgets.Display
  class MyProgressMonitorDialog (parent : Shell) extends ProgressMonitorDialog(parent) {
    import org.eclipse.swt.widgets.Button
    def getC () : Button = cancel
  }

  import org.eclipse.swt.events.{MouseListener, MouseEvent}
  private var p : IProgressMonitor = null
  private var pmd : MyProgressMonitorDialog = null
  private val nam = "Coq interaction: "
  private var title : String = ""

  def receive = {
    case ("START", n : String) =>
      title = n
      //Console.println("Starting progress monitor")
      if (p == null)
        if (CoqProgressMonitor.multistep)
          this.self.tell("REALLY")
        else
          CoqProgressMonitor.timer.tell(("START", CoqProgressMonitor.tick))
      else
        Display.getDefault.asyncExec(
          new Runnable() {
            def run() = {
              if (p != null)
                p.setTaskName(nam + ": " + n)
            }})
    case "REALLY" =>
      //assert(pmd == null)
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = {
            pmd = new MyProgressMonitorDialog(Display.getDefault.getActiveShell)
            pmd.setCancelable(true)
            pmd.open
            pmd.getC.addMouseListener(new MouseListener() {
              override def mouseDoubleClick (m : MouseEvent) : Unit = ()
              override def mouseDown (m : MouseEvent) : Unit = CoqTop.interruptCoq
              override def mouseUp (m : MouseEvent) : Unit = ()
            })
            p = pmd.getProgressMonitor
            p.beginTask(nam + title, IProgressMonitor.UNKNOWN)
          }})
    case "FINISHED" =>
      CoqProgressMonitor.tick = CoqProgressMonitor.tick + 1
      //Console.println("Finished progress monitor " + p)
      if (p != null && !CoqProgressMonitor.multistep) {
        val oldp = p
        val oldpmd = pmd
        p = null
        pmd = null
        Display.getDefault.asyncExec(
          new Runnable() {
            def run() = {
              oldp.done
              oldpmd.close
              //Clients should not call this method (the workbench calls this method at appropriate times). To have the workbench activate a part, use IWorkbenchPage.activate(IWorkbenchPart) instead.
              DocumentState.activeEditor.setFocus
            }
          })
      }
    case x => Console.println("fell through receive of CoqProgressMonitor " + x)
  }
}
*/

object JavaPosition extends CoqCallback {
  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  var editor : JavaEditor = null

  import org.eclipse.jdt.core.dom.MethodDeclaration
  var method : Option[MethodDeclaration] = None

  import org.eclipse.jface.text.Position
  import org.eclipse.jface.text.source.Annotation
  var processed : Option[Annotation] = None
  var processing : Option[Annotation] = None

  var specprocessed : List[Annotation] = List[Annotation]()
  var specprocessing : Option[Annotation] = None


  import org.eclipse.jface.text.IDocument
  def getDoc () : IDocument = {
    if (editor != null) {
      val prov = editor.getDocumentProvider
      prov.getDocument(editor.getEditorInput)
    } else null
  }

  def getProj () : CoqJavaProject = {
    val doc = getDoc
    if (doc != null)
      EclipseTables.DocToProject(doc)
    else
      null
  }

  import org.eclipse.ui.IFileEditorInput
  def getCoqString () : Option[String] = {
    val proj = getProj
    if (proj != null) {
      //proj.proveMethod(-1)
      proj.getCoqString
    } else None
  }

  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.IMarker
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqProofCompleted() =>
        if (editor != null) {
          Console.println("proof completed - not sending Qed? " + CoqStepNotifier.active)
          if (! CoqStepNotifier.active) {
            while (! DocumentState.readyForInput) { } //XXX: bad busy loop
            CoqStepAction.doit()
          }
        }
      case CoqTheoremDefined(x) =>
        if (editor != null && x.startsWith("valid_")) { // + method.get.id)) {
          val doc = getDoc
          //what about specifications!?
          val spos = 0 //doc.getLineOffset(method.get.pos.line - 1)
          val epos = 0 //doc.getLineOffset(method.get.body.last.pos.line + 1) - 1
          markproven(spos, epos - spos)
          //generate proof certificate IF last method!
          retract
          ActionDisabler.enableStart
        }
      case CoqError(m, n, s, l) =>
        unmark
        if (editor != null) {
          //have coq position, need java position!
          //might either be spec or proof script (or neither of them)
          val proj = getProj
          val doc = getDoc
          proj.proofShell match {
            case None =>
              //spec!
              val poff = DocumentState.position //- proj.program.get.getSpecOffset + 1 + s
              Console.println("have a spec here... " + poff)
              mark(n, -1, 0, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
/*              val ast = findSpecForCoqOffset(poff)
              ast match {
                case Some(as) =>
                  val soff = doc.getLineOffset(as.pos.line - 1)
                  val content = doc.get(soff, doc.getLineLength(as.pos.line - 1))
                  val offset = content.drop(as.pos.column).indexOf(": ") + 2
                  val sss = s - as.getCoqPos.offset
                  val star = soff + offset + sss + as.pos.column
                  mark(n, star, l, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
                case None =>
              } */
            case Some(x) =>
              //proof!
              next match {
                case Some(st) =>
                  //not entirely correct computation... ("invariant:" and "frame:")
                  val star = s + st.getStartPosition + 3 //"<% "
                  Console.println("marking at " + star + " (s: " + s + " sp: " + st.getStartPosition + ")")
                  mark(n, star, l, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
                case None =>
                  mark(n, -1, 0, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
              }
          }
        }
      case CoqShellReady(monoton, tokens) =>
        if (monoton)
          reAnnotate(false, false)
        else
          if (CoqState.lastWasUndo)
            reAnnotate(false, true)
          else
            reAnnotate(true, true)
      case _ =>
    }
  }

  def markproven (s : Int, l : Int) = {
    //getProj.provenMethods ::= method.get
    markHelper("Verified", s, l, "dk.itu.sdg.kopitiam.provenmarker", IMarker.SEVERITY_ERROR) match {
      case Some(x) => proofmarkers ::= x
      case None =>
    }
    //CoqTop.writeToCoq("Backtrack " + ...)
  }

  def unmarkProofs () : Unit = {
    proofmarkers.foreach(_.delete)
    proofmarkers = List[IMarker]()
  }

  var markers : List[IMarker] = List[IMarker]()
  var proofmarkers : List[IMarker] = List[IMarker]()

  def unmark () : Unit = {
    markers.foreach(_.delete)
    markers = List[IMarker]()
  }

  def mark (message : String, spos : Int, len : Int, typ : String, severity : Int) : Unit = {
    markHelper(message, spos, len, typ, severity) match {
      case Some(x) => markers ::= x
      case None =>
    }
  }

  def markHelper (message : String, spos : Int, len : Int, typ : String, severity : Int) : Option[IMarker] = {
    val file = editor.getEditorInput
    if (file.isInstanceOf[IFileEditorInput]) {
      val rfile = file.asInstanceOf[IFileEditorInput].getFile
      val mark = rfile.createMarker(typ)
      mark.setAttribute(IMarker.MESSAGE, message)
      mark.setAttribute(IMarker.LOCATION, rfile.getName)
      if (spos >= 0) {
        mark.setAttribute(IMarker.CHAR_START, spos)
        mark.setAttribute(IMarker.CHAR_END, spos + len)
      }
      mark.setAttribute(IMarker.TRANSIENT, true)
      mark.setAttribute(IMarker.SEVERITY, severity)
      Some(mark)
    } else {
      Console.println("no fileeditorinput " + file)
      None
    }
  }

  def retractModel () : Unit = {
    val proj = getProj
    if (proj != null) {
      proj.modelShell = None
      proj.proofShell = None
      proj.modelNewerThanSource = true
    }
  }

  import org.eclipse.swt.widgets.Display
  def retract () : Unit = {
    if (editor != null) {
      Console.println("hello my friend, NONONONO")
      //method = None
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val annmodel = prov.getAnnotationModel(editor.getEditorInput)
      getProj.javaNewerThanSource = true
      annmodel.connect(doc)
      processed match {
        case Some(x) => annmodel.removeAnnotation(x)
        case None =>
      }
      processed = None
      processing match {
        case Some(x) => annmodel.removeAnnotation(x)
        case None =>
      }
      processing = None
      specprocessing match {
        case Some(x) => annmodel.removeAnnotation(x)
        case None =>
      }
      specprocessing = None
      specprocessed.foreach(annmodel.removeAnnotation(_))
      specprocessed = List[Annotation]()
      annmodel.disconnect(doc)
      PrintActor.deregister(JavaPosition)
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = { editor.getViewer.invalidateTextPresentation }})
    }
  }

  var proofoffset : Int = -1

  import org.eclipse.jdt.core.dom.Statement
  var cur : Option[Statement] = None
  var next : Option[Statement] = None

  import org.eclipse.jdt.core.dom.{EmptyStatement, WhileStatement, IfStatement, Block}
  import scala.collection.immutable.Stack

  def getLastCoqStatement () : Option[Statement] = {
    assert(next == None)
    var todo : Stack[Statement] = Stack[Statement]()
    todo = todo.push(method.get.getBody)
    while (!todo.isEmpty && next == None) {
      val st = todo.top
      todo = todo.pop
      st match {
        case x : Block =>
          todo = todo.pushAll(scala.collection.JavaConversions.asBuffer(x.statements).map(_.asInstanceOf[Statement]))
        case x : IfStatement =>
          todo = todo.push(x.getThenStatement)
          todo = todo.push(x.getElseStatement)
        case x : WhileStatement =>
          todo = todo.push(x.getBody)
        case x : Statement =>
          val sh = x.getProperty(EclipseJavaASTProperties.coqShell)
          if (sh != null)
            cur match {
              case None => //not sure what to do here....
              case Some(y) =>
                if (x != y) {
                  Console.println("found sth exciting: " + x + ", which is not " + y)
                  next = Some(x)
                }
                val she = y.getProperty(EclipseJavaASTProperties.coqShell).asInstanceOf[CoqShellTokens]
                val sh1 = sh.asInstanceOf[CoqShellTokens]
                if (she == sh1)
                  Console.println("CST are the same " + she)
                else
                  Console.println("CST are different: " + she + " vs " + sh1)
            }
      }
    }
    next
  }

  def getCoqCommand () : Option[String] = {
    val prov = editor.getDocumentProvider
    val doc = prov.getDocument(editor.getEditorInput)
    var res : Option[String] = None
    var todo : Stack[Statement] = Stack[Statement]()
    var active : Boolean = (cur == None)
    todo = todo.push(method.get.getBody)
    while (res == None && !todo.isEmpty) {
      val nextst = todo.top
      todo = todo.pop
      res = nextst match {
        case x : EmptyStatement =>
          if (active) {
            val script = doc.get(x.getStartPosition, x.getLength)
            Console.println("script is " + script + " (size: " + script.length + ")")
            val con =
              if (script.contains("invariant:")) {
                val i1 = script.indexOf(":")
                val i2 = script.indexOf("frame:")
                val i3 = if (i2 == -1) script.length - 3 else i2
                val i = script.substring(i1 + 1, i3).trim
                val f =
                  if (i2 == -1)
                    "<true>"
                  else
                    script.substring(i3 + 6, script.length - 3).trim
                Console.println("inv: " + i1 + " " + i2 + " " + i3 + " f " + f)
                "forward (" + i + ") (" + f + ")."
              } else
                script.drop(2).dropRight(2).trim
            Console.println("found ES: " + con)
            next = Some(x)
            Some(con)
          } else None
        case x : WhileStatement =>
          todo = todo.push(x.getBody)
          None
        case x : IfStatement =>
          todo = todo.push(x.getElseStatement)
          todo = todo.push(x.getThenStatement)
          None
        case x : Block =>
          todo = todo.pushAll(scala.collection.JavaConversions.asBuffer(x.statements).map(_.asInstanceOf[Statement]).reverse)
          None
        case _ =>
          Console.println("found nth")
          None
      }
      if (!active && cur.get == nextst)
        active = true
    }
    res
  }


  def reAnnotate (proc : Boolean, undo : Boolean) : Unit = {
    Console.println("reannotate called with proc " + proc + " undo " + undo + " proofoff " + proofoffset + " method " + method + " editor " + editor)
    //4 cases:
    // #t #f =>                  remove nothing, mark yellow
    // #t #t => problem marker - remove yellow, mark nothing
    // #f #t => real undo -      remove last green, remark green
    // #f #f => processed!       remove yellow & green, mark green
    if (editor != null && method != None) {
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      if (proofoffset != -1) {
        //we're in proof mode!

        val start = method.get.getStartPosition

        if (!proc && undo)
          cur match {
            case Some(x) =>
              Console.println("removing coqshell property")
              x.setProperty(EclipseJavaASTProperties.coqShell, null)
            case None =>
          }

        Console.println(" reAnn " + next + " proc " + proc + " undo " + undo)
        if (next != None && !proc) { // && !undo) {
          Console.println("  ass " + cur + " now " + next)
          cur = next
          next = None
        }

        val end =
          cur match {
            case None =>
              val c1 = doc.getLineOfOffset(start)
              doc.getLineOffset(c1 + 1) - 2
            case Some(x) =>
              //preserve current shell -- if success!
              if (!proc && !undo) {
                Console.println("preserving for " + x + " shell " + CoqState.getShell)
                x.setProperty(EclipseJavaASTProperties.coqShell, CoqState.getShell)
              }
              x.getStartPosition + x.getLength
          }

        val annmodel = prov.getAnnotationModel(editor.getEditorInput)
        annmodel.connect(doc)

        if ((proc && undo) || (!proc && !undo)) {
          processing match {
            case Some(x) =>
              Console.println("removing processing")
              annmodel.removeAnnotation(x)
            case None =>
          }
          processing = None
        }
        if ((!proc && undo) || (!proc && !undo)) {
          processed match {
            case Some(x) => {
              Console.println("removing processed")
              annmodel.removeAnnotation(x)
              if (undo)
                Display.getDefault.asyncExec(
                  new Runnable() {
                    def run() = { editor.getViewer.invalidateTextPresentation }})
            }
            case None =>
          }
          processed = None
        }

        if (!(proc && undo) && (end > start)) {
          val txt =
            if (proc)
              "dk.itu.sdg.kopitiam.processing"
            else
              "dk.itu.sdg.kopitiam.processed"
          val l1 = doc.getLineOfOffset(start)
          val c1 = start - doc.getLineOffset(l1)
          val l2 = doc.getLineOfOffset(end)
          val c2 = end - doc.getLineOffset(l2)
          Console.println("adding " + txt + " annotation for: " + start + " -- " + end + " (len: " + (end - start) + "): (" + l1 + ", " + c1 + ") -- (" + l2 + ", " + c2 + ")")
          val sma = new Annotation(txt, false, "Proof")
          annmodel.addAnnotation(sma, new Position(start, end - start))
          if (proc)
            processing = Some(sma)
          else
            processed = Some(sma)
        }
        annmodel.disconnect(doc)
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
  import org.eclipse.ui.{IWorkbenchWindow,IEditorPart}
  import org.eclipse.ui.texteditor.{ITextEditor,IDocumentProvider,AbstractTextEditor}
  import org.eclipse.jface.text.{IDocument,ITextSelection}

  var window : IWorkbenchWindow = null

  def getCaretPosition () : Int = {
    val sel = DocumentState.activeEditor.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
    //Console.println("cursor position is " + sel.getLength + " @" + sel.getOffset)
    sel.getOffset
  }

  import org.eclipse.ui.{IFileEditorInput, PlatformUI}
  import org.eclipse.core.resources.IResource
  def getProjectDir () : String = {
    val editor =
      if (DocumentState.activeEditor != null)
        DocumentState.activeEditor
      else if (JavaPosition.editor != null)
        JavaPosition.editor
      else {
        Console.println("no active editor")
        null
      }
    if (editor != null) {
      val input = editor.getEditorInput
      val res : Option[IResource] =
        if (input.isInstanceOf[IFileEditorInput])
          Some(input.asInstanceOf[IFileEditorInput].getFile)
        else
          None
      res match {
        case Some(r) => r.getProject.getLocation.toOSString
        case None =>
          Console.println("shouldn't happen - trying to get ProjectDir from " + input + ", which is not an IFileEditorInput")
          ""
      }
    } else
      ""
  }

  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.dialogs.MessageDialog
  def warnUser (title : String, message : String) : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = MessageDialog.openWarning(Display.getDefault.getActiveShell, title, message)
      })
  }


  import org.eclipse.core.resources.{IMarker, IResource}

  def mark (text : String, severity : Int = IMarker.SEVERITY_ERROR, advance : Boolean = false, off : Int = 0, len : Int = 0) : Unit = {
    if (DocumentState.activeEditor != null) {
      val file = DocumentState.resource
      var spos = if (advance) DocumentState.sendlen + DocumentState.position + 1 else DocumentState.position + 1
      val con = DocumentState.content
      while ((con(spos) == '\n' || con(spos) == ' ' || con(spos) == '\t') && spos < con.length)
        spos += 1
      spos += off
      val epos =
        if (advance)
          spos + 1
        else if (len > 0)
          spos + len
        else
          DocumentState.position + DocumentState.oldsendlen - 1
      val marker = file.createMarker(IMarker.PROBLEM)
      marker.setAttribute(IMarker.MESSAGE, text)
      marker.setAttribute(IMarker.LOCATION, file.getName)
      val commentoff = CoqTop.computeCommentOffset(con.drop(DocumentState.position), spos - DocumentState.position)
      marker.setAttribute(IMarker.CHAR_START, spos + commentoff)
      marker.setAttribute(IMarker.CHAR_END, epos + commentoff) //for tha whitespace
      marker.setAttribute(IMarker.SEVERITY, severity)
      marker.setAttribute(IMarker.TRANSIENT, true)
    }
  }

  def unmarkReally () : Unit = {
    if (DocumentState.activeEditor != null)
      DocumentState.resource.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
  }

  def unmark () : Unit = {
    if (DocumentState.activeEditor != null) {
      val marks = DocumentState.resource.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
      marks.foreach(x => if (x.getAttribute(IMarker.SEVERITY, 0) == IMarker.SEVERITY_ERROR) x.delete)
    }
  }

  def maybeunmark (until : Int) : Unit = {
    if (DocumentState.activeEditor != null) {
      val marks = DocumentState.resource.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
      marks.foreach(x => if (x.getAttribute(IMarker.CHAR_START, 0) < until && x.getAttribute(IMarker.SEVERITY, 0) == IMarker.SEVERITY_ERROR) x.delete)
    }
  }
}

import dk.itu.sdg.util.KopitiamLogger
object DocumentState extends CoqCallback with KopitiamLogger {
  import org.eclipse.ui.IWorkbenchPart
  var activated : IWorkbenchPart = null

  var activeEditor : CoqEditor = null

  import org.eclipse.jface.text.IDocument
  def activeDocument () : IDocument = {
    if (activeEditor != null)
      activeEditor.getDocumentProvider.getDocument(activeEditor.getEditorInput)
    else
      null
  }

  def resetState () : Unit = {
    val value = positionToShell.get(0)
    positionToShell.clear
    value match {
      case Some(x) => positionToShell += 0 -> x
      case None =>
    }
    position_ = 0
    sendlen = 0
    _content = None
    invalidateCoqMarker
  }

  import org.eclipse.ui.{IFileEditorInput,IURIEditorInput}
  import org.eclipse.core.resources.{IFile,IResource,ResourcesPlugin}
  import org.eclipse.core.runtime.Path
  def resource () : IFile = {
    if (activeEditor != null) {
      val input = activeEditor.getEditorInput
      if (input.isInstanceOf[IFileEditorInput])
        input.asInstanceOf[IFileEditorInput].getFile
      else if (input.isInstanceOf[IURIEditorInput]) {
        val path = input.asInstanceOf[IURIEditorInput].getURI.getPath
        val ws = ResourcesPlugin.getWorkspace();
        val project = ws.getRoot().getProject("External Files");
        if (!project.exists())
          project.create(null);
        if (!project.isOpen())
          project.open(null);
        val location = new Path(path);
        val file = project.getFile(location.lastSegment());
        if (! file.exists)
          file.createLink(location, IResource.NONE, null);
        file
      } else null
    } else null
  }

  var _content : Option[String] = None

  def content () : String = {
    _content match {
      case None =>
        Console.println("no content. activeeditor is " + activeEditor + " javapos is " + JavaPosition.editor)
        if (activeEditor != null)
          _content = Some(activeDocument.get)
        else if (JavaPosition.editor != null) {
          Console.println("calling JP.getCS here...")
          _content = JavaPosition.getCoqString
        }
        _content.getOrElse("  ") //not happy with this hack
      case Some(x) =>
        x
    }
  }

  def nextCommand () : Option[String] = {
    val cont = content.drop(DocumentState.position)
    if (cont.length > 0) {
      val eoc = CoqTop.findNextCommand(cont)
      if (eoc > 0) {
        val cmd = cont.take(eoc).trim
        sendlen = eoc
        Some(cmd)
      } else None
    } else None
  }



  import scala.collection.mutable.HashMap
  var positionToShell : HashMap[Int,CoqShellTokens] = new HashMap[Int,CoqShellTokens]()

  var sendlen : Int = 0
  var until : Int = -1

  import org.eclipse.core.resources.IMarker
  import org.eclipse.core.runtime.CoreException
  private var coqmarker : IMarker = null

  def invalidateCoqMarker () : Unit = {
    if (coqmarker != null)
      coqmarker.delete
    coqmarker = null
  }

  var position_ : Int = 0
  def position : Int = position_
  def position_= (x : Int) {
    if (activeEditor != null) {
      //Console.println("new pos is " + x + " (old was " + position_ + ")");
      if (coqmarker == null) {
        val file = resource
        coqmarker = file.createMarker(IMarker.BOOKMARK)
        coqmarker.setAttribute(IMarker.MESSAGE, "coq position")
        coqmarker.setAttribute(IMarker.LOCATION, file.getName)
        coqmarker.setAttribute(IMarker.TRANSIENT, true)
        coqmarker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO)
      }
      try {
        coqmarker.setAttribute(IMarker.CHAR_START, x)
        coqmarker.setAttribute(IMarker.CHAR_END, x - 1) //at dot, not whitespace
        position_ = x
      } catch {
        case e : CoreException =>
          Console.println("caught CoreException")
          invalidateCoqMarker
          position = x
      }
    } else
      position_ = x
    //Console.println("position updated to " + x)
  }

  private var _readyForInput : Boolean = false
  def readyForInput () : Boolean = { _readyForInput }
  def setBusy () : Unit = { _readyForInput = false }

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(monoton, token) =>
        Console.println(" DS dispatch: " + monoton)
        if (monoton) {
          commit
          if (positionToShell.get(position) == None) {
            //Console.println("filling table (" + positionToShell.keys.toList.length + ") [" + position + "]: " + token)
            positionToShell += position -> token
          }
        } else
          undo
        _readyForInput = true
      case y =>
    }
  }

  var oldsendlen : Int = 0
  var reveal : Boolean = true
  var autoreveal : Boolean = false

  import org.eclipse.swt.widgets.Display
  def undo () : Unit = {
    //Console.println("undo (@" + position + ", sendlen: " + sendlen + ")
    if (sendlen != 0) {
      if (CoqState.lastWasUndo) {
        val start = scala.math.max(position - sendlen, 0)
        if (activeEditor != null) {
          activeEditor.addAnnotations(start, 0)
          activeEditor.invalidate
          if (reveal)
            Display.getDefault.asyncExec(
              new Runnable() {
                def run() = {
                  activeEditor.selectAndReveal(start, 0)
                }
              })
        }
        if (autoreveal) {
          reveal = true
          autoreveal = false
        }
        position = start
        sendlen = 0
      } else { //just an error
        //Console.println("undo: barf")
        //kill off process colored thingies
        if (activeEditor != null) {
          activeEditor.addAnnotations(position, 0)
          activeEditor.invalidate
        }
        oldsendlen = sendlen
        sendlen = 0
      }
    }
  }

  def process () : Unit = {
    if (activeEditor != null)
      activeEditor.addAnnotations(position, scala.math.max(until - position, sendlen))
    JavaPosition.reAnnotate(true, false)
  }

  def processUndo () : Unit = {
    if (activeEditor != null) {
      activeEditor.addAnnotations(position, 0)
      activeEditor.invalidate
    }
    if (position == 0)
      JavaPosition.retract
    else
      JavaPosition.reAnnotate(true, true)
  }

  private def commit () : Unit = {
    //Console.println("commited (@" + position + ", " + sendlen + "): " + positionToShell.contains(position))
    if (sendlen != 0) {
      //Console.println("commited - and doing some work")
      val end = scala.math.min(sendlen, content.length - position)
      position += end
      sendlen = 0
      //XXX: that's wrong! sendlen is 0!!!!
      if (activeEditor != null) {
        activeEditor.addAnnotations(position, scala.math.max(until - position, sendlen))
        if (reveal)
          Display.getDefault.asyncExec(
            new Runnable() {
              def run() = {
                activeEditor.selectAndReveal(position, 0)
              }
            })
      }
    }
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

import org.eclipse.ui.part.ViewPart

class GoalViewer extends ViewPart {
  import org.eclipse.swt.widgets.{Composite,Text}
  import org.eclipse.swt.SWT
  import org.eclipse.swt.layout.{FormData,FormLayout,FormAttachment}
  import org.eclipse.swt.graphics.{Color,RGB,Rectangle}
  import org.eclipse.swt.widgets.{Display,Sash,Listener,Event,TabFolder,TabItem}
//  import org.eclipse.swt.custom.{CTabFolder,CTabItem}


  var context : Text = null
  var goals : TabFolder = null
  var subgoals : List[TabItem] = List[TabItem]()
  var subgoalTexts : List[Text] = List[Text]()
  var comp : Composite = null

  /*
   * |---------------------|       -
   * | context             |       | comp (FormLayout)
   * |---------------------| sash  |
   * | goals (TabFolder)   |       |
   * |---------------------|       -
   */

  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new FormLayout())

    context = new Text(comp, SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)

    goals = new TabFolder(comp, SWT.NONE)

    val sash = new Sash(comp, SWT.HORIZONTAL)

    //sash
    val contextData = new FormData()
    contextData.left = new FormAttachment(0, 0)
    contextData.right = new FormAttachment(100, 0)
    contextData.top = new FormAttachment(0, 0)
    contextData.bottom = new FormAttachment(sash, 0)
    context.setLayoutData(contextData)

    val goalData = new FormData()
    goalData.left = new FormAttachment(0, 0)
    goalData.right = new FormAttachment(100, 0)
    goalData.top = new FormAttachment(sash, 0)
    goalData.bottom = new FormAttachment(100, 0)
    goals.setLayoutData(goalData)

    val limit = 20
    val percent = 50
    val sashData = new FormData()
    sashData.left = new FormAttachment(0, 0)
    sashData.right = new FormAttachment(100, 0)
    sashData.top = new FormAttachment(percent, 0)
    sash.setLayoutData(sashData)
    sash.addListener(SWT.Selection, new Listener () {
      def handleEvent (e : Event) = {
        val sashRect : Rectangle = sash.getBounds()
        val shellRect : Rectangle = comp.getClientArea()
        val top = shellRect.height - sashRect.height - limit
        e.y = scala.math.max(scala.math.min(e.y, top), limit)
        if (e.y != sashRect.y)  {
          sashData.top = new FormAttachment (0, e.y)
          comp.layout()
        }
      }
    });

    CoqOutputDispatcher.goalviewer = this
  }

  def clear () : Unit = {
    writeGoal("", List[String]())
  }

  def writeGoal (assumptions : String, sgoals : List[String]) : Unit = {
    Display.getDefault.asyncExec(
      new Runnable() {
        def run() =
          if (! comp.isDisposed) {
            context.setText(assumptions)
            //Console.println("sgoals: " + sgoals.length + " subgoals: " + subgoals.length)
            if (sgoals.length < subgoals.length) {
              //drop some, but keep one!
              val (stay, leave) = subgoals.splitAt(sgoals.length)
              val (stayT, leaveT) = subgoalTexts.splitAt(sgoals.length)
              subgoals = stay
              subgoalTexts = stayT
              leaveT.foreach(_.setText(""))
              leaveT.foreach(_.dispose)
              leave.foreach(_.dispose)
              goals.reskin(SWT.ALL)
            } else if (sgoals.length > subgoals.length) {
              //add some
              val tomake = sgoals.length - subgoals.length
              var index : Int = 0
              while (index < tomake) {
                val ti = new TabItem(goals, SWT.NONE)
                subgoals = subgoals ++ List(ti)
                ti.setText((subgoals.length - 1).toString)
                val te = new Text(goals, SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)
                subgoalTexts = subgoalTexts ++ List(te)
                ti.setControl(te)
                index = index + 1
              }
            }
            sgoals.indices.foreach(x => subgoalTexts(x).setText(sgoals(x)))
            comp.layout
          }
        })
  }

  def setFocus() : Unit = {
  //  viewer.getControl.setFocus
  }
}

object GoalViewer extends GoalViewer { }

import org.eclipse.ui.IStartup
class Startup extends IStartup {
//  import akka.actor.ActorSystem
  override def earlyStartup () : Unit = {
    Console.println("earlyStartup called")
    ActionDisabler.disableAll
    DocumentMonitor.init
    //val system = ActorSystem("Kopitiam")
    //CoqProgressMonitor.actor = system.actorOf(Props[CoqProgressMonitorImplementation], name = "ProgressMonitor")
    //CoqProgressMonitor.timer = system.actorOf(Props[MyTimer], name = "MyTimer")
    CoqTop.init
    PrintActor.register(DocumentState)
    CoqTop.coqpath = Activator.getDefault.getPreferenceStore.getString("coqpath") + System.getProperty("file.separator")
    ActionDisabler.initializationFinished
  }
}
