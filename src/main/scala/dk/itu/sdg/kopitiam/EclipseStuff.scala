/* (c) 2010-2012 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam
import dk.itu.sdg.util.KopitiamLogger

import org.eclipse.ui.editors.text.TextEditor

import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitEditor

class SimpleJavaEditor extends CompilationUnitEditor {
  //setEditorContextMenuId("#CompilationUnitEditorContext"); //$NON-NLS-1$
  //setRulerContextMenuId("#CompilationUnitRulerContext"); //$NON-NLS-1$
  //setOutlinerContextMenuId("#CompilationUnitOutlinerContext"); //$NON-NLS-1$
  // don't set help contextId, we install our own help context
  //fSavePolicy= null;

  //fJavaEditorErrorTickUpdater= new JavaEditorErrorTickUpdater(this);
  //fCorrectionCommands= null;
  setDocumentProvider(SimpleJavaDocumentProvider);


  override def initializeEditor () : Unit = {
    Console.println("initializing SimpleJavaEditor!")
    setDocumentProvider(SimpleJavaDocumentProvider)
    Console.println("set document provider")
    super.initializeEditor()
    Console.println("called super")
    setDocumentProvider(SimpleJavaDocumentProvider)
    Console.println("initialized SimpleJavaEditor!")
  }
}

import org.eclipse.ui.editors.text.FileDocumentProvider

object SimpleJavaDocumentProvider extends FileDocumentProvider {
  import dk.itu.sdg.javaparser.JavaOutput
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.jface.text.IDocument
  import scala.collection.mutable.HashMap

  val docs : HashMap[String,IDocument] = new HashMap[String,IDocument]()

  override def getDefaultEncoding () : String = "UTF-8"

  override def getDocument (ele : Object) : IDocument = {
    Console.println("get document called on SimpleJavaDocumentProvider")
    assert(ele.isInstanceOf[FileEditorInput])
    val elem = ele.asInstanceOf[FileEditorInput]
    val nam = elem.getName
    if (docs.contains(nam))
      docs(nam)
    else {
      val document = super.getDocument(ele)
      //Console.println("getdocument received " + document.get)
      val newt = JavaOutput.parseandoutput(document.get)
      Console.println("SimpleJava getDocument called, translated")
      document.set(newt)
      docs += nam -> document
      document
    }
  }
}

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
  import scala.util.parsing.input.Position

  var javaSource : Option[IDocument] = None
  var coqModel : Option[IDocument] = None
  var coqSource : Option[IDocument] = None
  private var coqString : Option[String] = None //this will be code + spec
  var modelNewerThanSource : Boolean = true
  var javaNewerThanSource : Boolean = false
  var modelShell : Option[CoqShellTokens] = None
  var proofShell : Option[CoqShellTokens] = None

  //def -> offset + [length1, .., lengthn]
  var javaOffsets : HashMap[String, Pair[Position, List[Position]]] =
    new HashMap[String, Pair[Position, List[Position]]]()
  var coqOffsets : HashMap[String, Pair[Int, List[Pair[Int,Int]]]] =
    new HashMap[String, Pair[Int, List[Pair[Int,Int]]]]()
  var specOffsets : HashMap[String, Pair[List[Position], Pair[Int, List[Pair[Int,Int]]]]] =
    new HashMap[String, Pair[List[Position], Pair[Int, List[Pair[Int,Int]]]]]()
  var proofOffset : Int = 0
  var specOffset : Int = 0

  var methods : HashMap[String, String] = new HashMap[String, String]()
  var provenMethods : List[String] = List[String]()

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
  def proveMethod (sourceline : Int) : Unit = {
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
    Console.println("provemethod called with " + sourceline + " modelnewer: " + modelNewerThanSource + " javanewer: " + javaNewerThanSource + " modelshell " + modelShell + " proofshell " + proofShell)
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
              JavaPosition.mark("Please write a model file for this java file with a Module named " + basename + "_model, which is used to prove the java code.", 0, 10, IMarker.PROBLEM, IMarker.SEVERITY_WARNING)
            else
              open = true
          }
        case Some(x) =>
      }
      CoqCommands.doLater(() => {
        Console.println("activating model editor")
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
      if (! (javaNewerThanSource || coqString == None)) {
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
    if (javaNewerThanSource || coqString == None) {
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
        val fei = JavaPosition.editor.getEditorInput
        if (fei.isInstanceOf[IFileEditorInput]) {
          Console.println("translating file....")
          JavaPosition.unmark
          TranslateAction.translate(fei.asInstanceOf[IFileEditorInput].getFile, false)
        } else
          Console.println("fei not a IFEI")
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
          DocumentState._content = coqString
          PrintActor.register(JavaPosition)
          CoqStepAllAction.doit
        case Some(x) =>
          if (x.globalStep < CoqState.getShell.globalStep) {
            DocumentState.setBusy
            Console.println("backtracking to proofshell " + x)
            CoqTop.writeToCoq("Backtrack " + x.globalStep + " 0 " + CoqState.getShell.context.length + ".")
            DocumentState.position = coqString.get.length
          } else
            CoqCommands.step
      }
    })
    CoqCommands.doLater(() => {
      if (proofShell == None) {
        Console.println("preserving proof shell: " + CoqState.getShell)
        proofShell = Some(CoqState.getShell)
      }
      //Console.println("last closure with " + sourceline)
      val nam =
        if (sourceline == -1)
          if (coqOffsets.contains(JavaPosition.name))
            JavaPosition.name
          else {
            //Console.println("wasn't too lucky -- name is not in coqOffsets and sourceline is negative")
            ""
          }
        else {
          //try to find sourceline (+ 1) in javaOffsets, get name!
          var distName : Pair[String, Int] = ("", Int.MaxValue)
          for (x <- javaOffsets.keys) {
            //why 4? -- well, 1 is for counting from 1 instead of 0,
            // the 3 others are for spec!
            //lets filter ghosts
            //Console.println("oeff: " + (javaOffsets(x)._1.line - 4))
            val dist = sourceline - (javaOffsets(x)._1.line - 4)
            //look into barf
            if (dist >= 0 && distName._2 > dist) {
              Console.println("maybe verifying: " + x + " distance is " + dist)
              distName = (x, dist)
            }
          }
          //Console.println("found an approximation: " + distName)
          distName._1
        }
      //story so far: model is now updated, java might be newly generated!
      if (nam != "") {
        var off = coqOffsets(nam)._1 + proofOffset
        //Console.println("woosh woosh -- going to " + nam + " whose off is " + off)
        JavaPosition.name = nam
        DocumentState._content = getCoqString
        PrintActor.register(JavaPosition)
        if (DocumentState.position < off)
          CoqStepUntilAction.reallydoit(off)
      } else
        CoqCommands.step
    })
  }

  def setCoqString (s : Option[String]) : Unit = {
    coqString = s
  }

  def getCoqString () : Option[String] = {
    coqString match {
      case None => None
      case Some(x) => Some(x + "\n" + methods(JavaPosition.name))
    }
  }

  def updateSpecOffsets (offc : Pair[Int,Int], name : Option[String]) : Unit = {
    if (offc != (0, 0)) {
      //Console.println("offc is now " + offc)
      for (x <- specOffsets.keys) {
        //Console.println(" competing with " + proj.specOffsets(x)._2._1)
        val n1 =
          if (specOffsets(x)._2._1 > offc._1) {
            //Console.println("updated for " + x + ": from " + specOffsets(x)._2._1 + " to " + (specOffsets(x)._2._1 + offc._2))
            specOffsets(x)._2._1 + offc._2
          } else
            specOffsets(x)._2._1
        specOffsets = specOffsets + (x -> (specOffsets(x)._1, (n1, specOffsets(x)._2._2)))
      }
      
      name match {
        case None =>
        case Some(x) =>
          val offf = offc._1 - specOffsets(x)._2._1
          val n2 = specOffsets(x)._2._2.map(p => {
            if (p._1 > offf) {
              val n = (p._1 + offc._2, p._2)
              //Console.println("up: " + p + " -> " + n)
              n
            } else if (p._1 == offf) {
              val n = (p._1, p._2 + offc._2)
              //Console.println("up: " + p + " -> " + n)
              n
            } else
              p
          })
        specOffsets = specOffsets + (x -> (specOffsets(x)._1, (specOffsets(x)._2._1, n2)))
      }
      //Console.println("adjusting proof offset from " + proofOffset + " to " + (proofOffset + offc._2))
      proofOffset = proofOffset + offc._2
    }
  }

  def updateCoqOffsets (offc : Pair[Int,Int], name : Option[String]) : Unit = {
    if (offc != (0, 0)) {
      name match {
        case None =>
        case Some(x) =>
          val offf = offc._1 - coqOffsets(x)._1
          val n2 = coqOffsets(x)._2.map(p => {
            if (p._1 > offf) {
              val n = (p._1 + offc._2, p._2)
              Console.println("up: " + p + " -> " + n)
              n
            } else if (p._1 == offf) {
              val n = (p._1, p._2 + offc._2)
              Console.println("up: " + p + " -> " + n)
              n
            } else
              p
          })
        coqOffsets = coqOffsets + (x -> (coqOffsets(x)._1, n2))
      }
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

import org.eclipse.jface.text.source.AnnotationPainter.IDrawingStrategy
class ProofDrawingStrategy extends IDrawingStrategy with EclipseUtils {
  import org.eclipse.jface.text.source.Annotation
  import org.eclipse.swt.graphics.{GC, Color}
  import org.eclipse.swt.custom.{StyledText, StyleRange}

  def draw (ann : Annotation, gc : GC, text : StyledText, off : Int, len : Int, color : Color) = {
    val col =
      if (ann.getType == "dk.itu.sdg.kopitiam.processed")
        getPrefColor("coqSentBg")
      else if (ann.getType == "dk.itu.sdg.kopitiam.processing")
        getPrefColor("coqSentProcessBg")
      else
        null
    text.replaceStyleRanges(off, len, Array(new StyleRange(off, len, null, col)))
  }
}

object JavaPosition extends CoqCallback {
  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor

  var editor : JavaEditor = null
  var index : Int = -1
  var name : String = ""
  var active : Boolean = false
  var spec : Boolean = false

  import org.eclipse.jface.text.Position
  import org.eclipse.jface.text.source.Annotation

  var processed : Option[Annotation] = None
  var processing : Option[Annotation] = None

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
      proj.proveMethod(-1)
      proj.getCoqString
    } else None
  }

  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.IMarker
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqProofCompleted() =>
        if (editor != null && active) {
          Console.println("got proof completed")
          val proj = getProj
          Console.println("bah at " + proj.javaOffsets(name)._2.length + " idx " + index)
          if (proj.javaOffsets(name)._2.length == index) {
            active = false //to prevent catches down there
            //send Qed
            Console.println("sending or not ? " + CoqStepNotifier.active)
            if (! CoqStepNotifier.active) {
              while (! DocumentState.readyForInput) { } //XXX: bad busy loop
              CoqStepAction.doit()
            }
          }
        }
      case CoqTheoremDefined(x) =>
        if (editor != null && x.startsWith("valid_" + name)) {
          val doc = getDoc
          val locs = getProj.javaOffsets(name)
          val spos = doc.getLineOffset(locs._1.line - 1)
          val epos = doc.getLineOffset(locs._2(locs._2.length - 1).line + 2 - 1) - 1
          markproven(spos, epos - spos)
          active = true //to force the retract
          retract
          ActionDisabler.enableStart
        }
      case CoqError(m, n, s, l) =>
        unmark
        if (editor != null) {
          if (active) {
            val doc = getDoc
            val javapos = getProj.javaOffsets(name)._2(index)
            val soff = doc.getLineOffset(javapos.line - 1)
            val content = doc.get(soff, doc.getLineLength(javapos.line - 1))
            val specialoff =
              if (content.contains("invariant: ")) //LI! beware!
                2 //most likely wrong for frame... but need more structure for that
              else
                0
            val star = s + specialoff + soff + javapos.column + 2 //<%
            mark(n, star, l, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
/*          } else if (spec) {
            val doc = getDoc
            val javaPos = getProj.specOffsets(name)._1
            val coqOffs = getProj.specOffsets(name)._2._2
            val (line, off) =
              if (s > coqOffs(0)._1 && s < coqOffs(1)._1)
                //slightly wrong because of [A] and , being inserted (correct for first var)
                (javaPos(0).line - 1, javaPos(0).column - coqOffs(0)._1 + 4)
              else if (s > coqOffs(1)._1 && s < coqOffs(2)._1)
                (javaPos(1).line - 1, javaPos(1).column - coqOffs(1)._1)
              else //if (s > coqOffs(2)._1)
                (javaPos(2).line - 1, javaPos(2).column - coqOffs(2)._1)
            val lineoffset = doc.getLineOffset(line)
            val content = doc.get(lineoffset, doc.getLineLength(line))
            val offset = content.drop(off).indexOf(": ") + 2
            val star = lineoffset + off + offset + 2
            mark(n, star, l, IMarker.PROBLEM, IMarker.SEVERITY_ERROR) */
          } else if (DocumentState.position >= getProj.specOffset && DocumentState.position <= getProj.proofOffset) {
            Console.println("we've something wrong in the specs")
            var fnd : Boolean = false
            var fst : Boolean = true
            val doc = getDoc
            val reloff = DocumentState.position - getProj.specOffset + 1 + s
            for (x <- getProj.specOffsets.keys) {
              val coqOff = getProj.specOffsets(x)._2._1
              //Console.println("x is " + x + " reloff is " + reloff + " coqOff is " + coqOff + " thus: " + (reloff >= coqOff))
              if (reloff >= coqOff) {
                val specreloff = reloff - coqOff
                var i : Int = 0
                for (p <- getProj.specOffsets(x)._2._2) {
                  //Console.println("now we're talking with i " + i + " specreloff " + specreloff + " p " + p)
                  if (specreloff >= p._1 && specreloff <= p._1 + p._2) {
                    val javaPos = getProj.specOffsets(x)._1(i)

                    val lineoffset = doc.getLineOffset(javaPos.line - 1)
                    val content = doc.get(lineoffset, doc.getLineLength(javaPos.line - 1))
                    val offset = content.drop(javaPos.column).indexOf(": ") + 2
                    val specoff = specreloff - p._1
                    val lvoff =
                      if (content.contains("lvars"))
                        -4
                      else
                        0
                    val star = lineoffset + javaPos.column + offset + lvoff + specoff
                    fnd = true
                    mark(n, star, l, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
                  }
                  i = i + 1
                }
              }
            }
            if (!fnd)
              mark(n, -1, 0, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
          } else
            mark(n, -1, 0, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
        }
      case x =>
    }
  }

  def markproven (s : Int, l : Int) = {
    getProj.provenMethods ::= name
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

  def markPos (message : String, position : scala.util.parsing.input.Position) = {
    val doc = getDoc
    val pos = doc.getLineOffset(position.line - 1) + position.column
    val pos2 = doc.getLineOffset(position.line) - 1
    Console.println("marking java at " + pos + " with: " + message)
    mark(message, pos, pos2 - pos, IMarker.PROBLEM, IMarker.SEVERITY_ERROR)
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
    }
  }

  import org.eclipse.swt.widgets.Display
  def retract () : Unit = {
    if (editor != null && active) {
      active = false
      index = -1
      name = ""
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val annmodel = prov.getAnnotationModel(editor.getEditorInput)
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
      annmodel.disconnect(doc)
      PrintActor.deregister(JavaPosition)
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = { editor.getViewer.invalidateTextPresentation }})
    }
  }

  def getPos (i : Int, proj : CoqJavaProject, name : String) : Int = {
//elements : Pair[scala.util.parsing.input.Position,
//List[scala.util.parsing.input.Position]]) : Int = {
    val elements = proj.javaOffsets(name)
    if (i == -1)
      proj.specOffsets(name)._1(0).line
    else if (i >= elements._2.length)
      elements._2(elements._2.length - 1).line + 2
    else
      elements._2(i).line
  }

  def reAnnotate (proc : Boolean, undo : Boolean) : Unit = {
    Console.println("reannotate called with proc " + proc + " undo " + undo + " index is " + index + " active is " + active + " spec is " + spec)
    //4 cases:
    // #t #f =>                  remove nothing, mark yellow
    // #t #t => problem marker - remove yellow, mark nothing
    // #f #t => real undo -      remove last green, remark green
    // #f #f => processed!       remove yellow & green, mark green
    if (editor != null && name != "" && getProj != null && getProj.coqOffsets.contains(name)) {
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val proj = EclipseTables.DocToProject(doc)
      var doit : Boolean = active
      Console.println("lets evaluate: " + DocumentState.position + " coqoff " + proj.coqOffsets(name)._1 + " proof " + proj.proofOffset + " evals to " + (DocumentState.position < (proj.coqOffsets(name)._1 + proj.proofOffset - 2)))
      if (DocumentState.position < (proj.coqOffsets(name)._1 + proj.proofOffset - 2)) {
        if (active) {
          Console.println("deactivating, 'cause we're too low")
          active = false
          index = -1
        }
      } else if (! active) {
        Console.println("activating 'cause we're too high")
        active = true
        doit = true
      }
      Console.println("lets evaluate: " + DocumentState.position + " specoff " + proj.specOffsets(name)._2._1 + " spec " + proj.specOffset + " evals to " + (DocumentState.position == (proj.specOffsets(name)._2._1 + proj.specOffset - 2)))
      if (DocumentState.position == (proj.specOffsets(name)._2._1 + proj.specOffset - 2)) {
        if (proc && undo && spec) {
          //Console.println("awwwwwww, broke it. uncolor. retract! NOW!")
          doit = true
        } else if (proc) {
          //Console.println("working on tha spec real soon now, paint it black^Wyellow!")
          spec = true
          doit = true
        }
      } else if (DocumentState.position > (proj.specOffsets(name)._2._1 + proj.specOffset - 1) && spec) {
        //Console.println("done working on tha spec!, paint it blue^Wgreen")
        doit = true
      }
      if (doit) {
        val annmodel = prov.getAnnotationModel(editor.getEditorInput)
        annmodel.connect(doc)
        if (!spec) {
          val pos =
            if (!proc)
              getPos(-1, proj, name)
            else
              getPos(index, proj, name)
          val npos =
            scala.math.max(
              if (undo)
                index - 1
              else
                index + 1,
              -1)
          if (!proc)
            index = npos
          val nextpos : Int = getPos(npos, proj, name)
          if ((proc && undo) || (!proc && !undo)) {
            processing match {
              case Some(x) => annmodel.removeAnnotation(x)
              case None =>
            }
            processing = None
          }
          if ((!proc && undo) || (!proc && !undo)) {
            processed match {
              case Some(x) => {
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

          if (nextpos != pos) {
            val txt =
              if (proc)
                "dk.itu.sdg.kopitiam.processing"
              else
                "dk.itu.sdg.kopitiam.processed"
            val sma = new Annotation(txt, false, "Proof")
            
            val loff = doc.getLineOffset(pos - 1) //XXX: bah
            val finaloff = doc.getLineOffset(nextpos - 1) - 1
            if (! (proc && undo)) {
              annmodel.addAnnotation(sma, new Position(loff, finaloff - loff))
              if (proc)
                processing = Some(sma)
              else
                processed = Some(sma)
            }
          }
        }
        else //if (spec)
          if (proc && !undo) {
            processing match {
              case Some(x) => annmodel.removeAnnotation(x)
              case None =>
            }
            processing = None
            val sma = new Annotation("dk.itu.sdg.kopitiam.processing", false, "Proof")
            val loff = doc.getLineOffset(proj.specOffsets(name)._1(0).line - 1) //XXX: bah
            val finaloff = doc.getLineOffset(proj.specOffsets(name)._1(2).line) - 1
            annmodel.addAnnotation(sma, new Position(loff, finaloff - loff))
            processing = Some(sma)
          } else { //done with spec!
            processing match {
              case Some(x) => annmodel.removeAnnotation(x)
              case None =>
            }
            processing = None
            if (! undo) {
              val sma = new Annotation("dk.itu.sdg.kopitiam.processed", false, "Proof")
              val loff = doc.getLineOffset(proj.specOffsets(name)._1(0).line - 1) //XXX: bah
              val finaloff = doc.getLineOffset(proj.specOffsets(name)._1(2).line) - 1
              annmodel.addAnnotation(sma, new Position(loff, finaloff - loff))
              processed = Some(sma)
              spec = false
            } else
              Display.getDefault.asyncExec(
                new Runnable() {
                  def run() = { editor.getViewer.invalidateTextPresentation }})
          }
        annmodel.disconnect(doc)
      }
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
  import java.net.URI
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

  import scala.collection.mutable.HashMap
  var positionToShell : HashMap[Int,CoqShellTokens] = new HashMap[Int,CoqShellTokens]()

  var sendlen : Int = 0
  var until : Int = -1
  var realundo : Boolean = false

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

  def readyForInput () : Boolean = { _readyForInput }
  def setBusy () : Unit = { _readyForInput = false }
  private var _readyForInput : Boolean = false
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(monoton, token) =>
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
  import org.eclipse.jface.text.{ Region, TextPresentation }
  import org.eclipse.swt.custom.StyleRange

  var reveal : Boolean = true
  var autoreveal : Boolean = false

  import org.eclipse.swt.widgets.Display
  def undo () : Unit = {
    //Console.println("undo (@" + position + ", sendlen: " + sendlen + ") real: " + realundo)
    if (sendlen != 0) {
      if (realundo) {
        val start = scala.math.max(position - sendlen, 0)
        realundo = false
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
        JavaPosition.reAnnotate(false, true)
      } else { //just an error
        //Console.println("undo: barf")
        //kill off process colored thingies
        if (activeEditor != null) {
          activeEditor.addAnnotations(position, 0)
          activeEditor.invalidate
        }
        oldsendlen = sendlen
        sendlen = 0
        JavaPosition.reAnnotate(true, true)
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
      JavaPosition.reAnnotate(false, false)
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
