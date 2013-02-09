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
  var javaSource : Option[IDocument] = None
  var coqModel : Option[IDocument] = None
  var modelNewerThanSource : Boolean = true
  var javaNewerThanSource : Boolean = true
  var ASTdirty : Boolean = false
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
  }

  def setDocument (doc : IDocument, name : String) : Unit = {
    if (name.equals(basename + ".java")) {
      assert(javaSource == None)
      javaSource = Some(doc)
    } else if (name.equals(basename + ".v")) {
      assert(coqModel == None)
      coqModel = Some(doc)
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

  import org.eclipse.ui.{IFileEditorInput, PlatformUI}
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.core.resources.{IFile, IMarker, IProject}
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jdt.core.dom.MethodDeclaration
  def proveMethod (meth : MethodDeclaration) : Unit = {
    proofShell match {
      case Some(x) =>
        if (x.globalStep > CoqState.getShell.globalStep)
          proofShell = None
      case None =>
    }
    Console.println("provemethod called! modelnewer: " + modelNewerThanSource + " javanewer: " + javaNewerThanSource + " proofshell " + proofShell)
    if (modelNewerThanSource) {
      modelNewerThanSource = false
      var model : IFile = null
      if (JavaPosition.editor == null)
        Console.println("this should not happen - no java editor...")
      else {
        val fei = JavaPosition.editor.getEditorInput
        if (fei.isInstanceOf[IFileEditorInput]) {
          val proj : IProject = fei.asInstanceOf[IFileEditorInput].getFile.getProject
          val maybemodel = proj.getFile(basename + "_model.v")
          if (maybemodel.exists)
            model = maybemodel
          else {
            val maybemodel = proj.getFile("src/" + basename + "_model.v")
            if (maybemodel.exists)
              model = maybemodel
          }
        } else {
          JavaPosition.mark("something went wrong reading the Java file", 0, 10, IMarker.PROBLEM, IMarker.SEVERITY_WARNING)
          return
        }
        if (model == null || ! model.exists) {
          JavaPosition.mark("Please write a model file for this Java file named '" + basename + "_model'.", 0, 10, IMarker.PROBLEM, IMarker.SEVERITY_WARNING)
          return
        }
      }
      new CoqCompileJob(model.getProject.getLocation.toFile, model.getName, true).schedule
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
          CoqStepAllAction.doitH
        case Some(x) =>
          val sh : CoqShellTokens = JavaPosition.method match {
            case None => x
            case Some(y) =>
              if (y == meth) {
                val sh = y.getProperty(EclipseJavaASTProperties.coqShell)
                if (sh != null && sh.isInstanceOf[CoqShellTokens])
                  sh.asInstanceOf[CoqShellTokens]
                else
                  x
              } else x
          }
          Console.println("have a PS: " + sh.globalStep + " < " + CoqState.getShell.globalStep)
          if (sh.globalStep < CoqState.getShell.globalStep) {
            JavaPosition.cur = None
            JavaPosition.next = None
            JavaPosition.emptyCoqShells
            JavaPosition.method = None
            DocumentState.setBusy
            Console.println("backtracking to shell " + sh)
            CoqTop.writeToCoq("Backtrack " + sh.globalStep + " 0 " + CoqState.getShell.context.length + ".")
          } else
            CoqCommands.step
      }
    })
    CoqCommands.doLater(() => {
      if (proofShell == None) {
        Console.println("preserving proof shell: " + CoqState.getShell)
        proofShell = Some(CoqState.getShell)
      }
      if (JavaPosition.method == None) {
        Console.println("assigning method to JP ")
        //story so far: model is now updated, java might be newly generated!
        JavaPosition.method = Some(meth)
        val prf = meth.getProperty(EclipseJavaASTProperties.coqProof)
        assert(prf != null)
        val p = prf.asInstanceOf[String]
        Console.println("p is " + p)
        DocumentState._content = Some(DocumentState._content.getOrElse("") + p)
        PrintActor.register(JavaPosition)
        CoqStepAllAction.doitH
      } else
        CoqCommands.step
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

object JavaPosition extends CoqCallback with EclipseJavaHelper with JavaASTUtils {
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

  import org.eclipse.jdt.core.dom.CompilationUnit
  def generateCertificate (c : CompilationUnit) : String = {
    //prog
    val pdef = c.getProperty(EclipseJavaASTProperties.coqDefinition).asInstanceOf[String]
    //spec
    val spec = c.getProperty(EclipseJavaASTProperties.coqSpecification).asInstanceOf[String]
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
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqProofCompleted() =>
        if (editor != null) {
          DocumentState.setBusy
          Console.println("writing qed")
          CoqTop.writeToCoq("Qed.")
        }
      case CoqTheoremDefined(x) =>
        if (editor != null && x.startsWith("valid_")) { // + method.get.id)) {
          //what about specifications!?
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

                  val edi = editor.getEditorInput.asInstanceOf[IFileEditorInput]
                  val trfi : IFile = edi.getFile.getProject.getFile(p.base_name + "Java.v")
                  if (trfi.exists)
                    trfi.delete(true, false, null)
                  trfi.create(null, IResource.NONE, null)
                  trfi.setCharset("UTF-8", null)
                  val bytes = c.getBytes("UTF-8")
                  val bs = new Array[Byte](bytes.length)
                  System.arraycopy(bytes, 0, bs, 0, bytes.length)
                  trfi.setContents(new ByteArrayInputStream(bs), IResource.NONE, null)

                  Console.println("received certificate: " + c)
                }
            }
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
          if (CoqState.lastWasUndo)
            reAnnotate(false, true)
          else
            reAnnotate(true, true)
      case _ =>
    }
  }

  import scala.collection.mutable.HashMap
  def markproven (s : Int, l : Int) = {
    //getProj.provenMethods ::= method.get
    val ps = proofScript(method.get)
    markHelper("Verified using: " + ps, s, l, "dk.itu.sdg.kopitiam.provenmarker", IMarker.SEVERITY_ERROR) match {
      case Some(x) =>
        proofmarkers += method.get.getName.getIdentifier -> x
      case None =>
    }
  }

  def unmarkProofs () : Unit = {
    for ((name, marker) <- proofmarkers) marker.delete
    proofmarkers.clear
  }

  var markers : List[IMarker] = List[IMarker]()
  val proofmarkers : HashMap[String, IMarker] = new HashMap[String, IMarker]()

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

  import org.eclipse.swt.widgets.Display
  def retract () : Unit = {
    val mn = (method != None)
    Console.println("retracting with " + editor + " and method? " + mn)
    if (editor != null && method != None) {
      Console.println("hello my friend, NONONONO")
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      unmarkProofs
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
      specprocessing match {
        case Some(x) => annmodel.removeAnnotation(x)
        case None =>
      }
      specprocessing = None
      specprocessed.foreach(annmodel.removeAnnotation(_))
      specprocessed = List[Annotation]()
      annmodel.disconnect(doc)
      cur = None
      next = None
      emptyCoqShells
      method = None
      if (getProj != null)
        getProj.proofShell = None
      PrintActor.deregister(JavaPosition)
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = { editor.getViewer.invalidateTextPresentation }})
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

  def emptyCoqShells () : Unit = {
    method match {
      case Some(x) =>
        x.setProperty(EclipseJavaASTProperties.coqShell, null)
        val clean : Statement => Option[Unit] = x => { x.setProperty(EclipseJavaASTProperties.coqShell, null); None }
        traverseAST(x, false, false, clean)
      case None =>
    }
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
  }

  def getASTbeforeOff (off : Int) : Option[Statement] = {
    Console.println("Called with off " + off)
    assert(next == None)
    getProj.program match {
      case None => None
      case Some(x) =>
        val n = findASTNode(x, off, 0)
        Console.println("n is " + n.getClass.toString + ": " + n)
        val m = findMethod(n)
        Console.println("Is the method the same? " + (m == method.get))
        if (m == method.get) {
          Console.println("YEP")
          var nx : Boolean = false

          val cb : Statement => Option[Statement] = x => {
            val r =
              if (nx) {
                val cs = x.getProperty(EclipseJavaASTProperties.coqShell)
                if (cs != null)
                  Some(x)
                else
                  None
              } else {
                x.setProperty(EclipseJavaASTProperties.coqShell, null)
                None
              }
            if (n == x)
              nx = true
            r
          }

          val r = traverseAST(m, false, true, cb)
          assert(r.size == 1)
          next = Some(r.first)
        }
    }
    next
  }

  def getLastCoqStatement () : Option[CoqShellTokens] = {
    assert(next == None)

    val find : Statement => Option[Statement] = x => {
      val sh = x.getProperty(EclipseJavaASTProperties.coqShell)
      if (sh != null)
        cur match {
          case None => None
          case Some(y) =>
            if (x != y)
              Some(x)
            else
              None
        } else None
    }
    val r = traverseAST(method.get, false, true, find)
    assert(r.size == 1 || r.size == 0)
    if (r.size == 1)
      next = Some(r.first)
    next match {
      case None =>
        val sh = method.get.getProperty(EclipseJavaASTProperties.coqShell)
        if (sh != null)
          Some(sh.asInstanceOf[CoqShellTokens])
        else
          JavaPosition.getProj.proofShell
      case Some(x) =>
        val sh = x.getProperty(EclipseJavaASTProperties.coqShell)
        assert(sh != null && sh.isInstanceOf[CoqShellTokens])
        Some(sh.asInstanceOf[CoqShellTokens])
    }
  }

  def copyProps (from : MethodDeclaration, to : MethodDeclaration) : Unit = {
    //implicit assumption is that structure up until cur is the same!
    var newcur : Option[Statement] = None
    var todof : Stack[Statement] = Stack[Statement]()
    todof = todof.push(from.getBody)
    var todot : Stack[Statement] = Stack[Statement]()
    todot = todot.push(to.getBody)
    to.setProperty(EclipseJavaASTProperties.coqShell, from.getProperty(EclipseJavaASTProperties.coqShell))
    var end : Boolean = false
    while (!end && !todof.isEmpty) {
      val nextfrom = todof.top
      todof = todof.pop
      val nextto = todot.top
      todot = todot.pop
      nextfrom match {
        case x : WhileStatement =>
          assert(nextto.isInstanceOf[WhileStatement])
          todof = todof.push(x.getBody)
          todot = todot.push(nextto.asInstanceOf[WhileStatement].getBody)
        case x : IfStatement =>
          assert(nextto.isInstanceOf[IfStatement])
          val y = nextto.asInstanceOf[IfStatement]
          val el = x.getElseStatement
          val elt = y.getElseStatement
          if (el != null)
            todof = todof.push(el)
          if (elt != null)
            todot = todot.push(elt)
          todof = todof.push(x.getThenStatement)
          todot = todot.push(y.getThenStatement)
        case x : Block =>
          assert(nextto.isInstanceOf[Block])
          todof = todof.pushAll(scala.collection.JavaConversions.asBuffer(x.statements).map(_.asInstanceOf[Statement]).reverse)
          todot = todot.pushAll(scala.collection.JavaConversions.asBuffer(nextto.asInstanceOf[Block].statements).map(_.asInstanceOf[Statement]).reverse)
        case x : Statement =>
          assert(x.getClass == nextto.getClass)
          val cs = x.getProperty(EclipseJavaASTProperties.coqShell)
          if (cs != null)
            nextto.setProperty(EclipseJavaASTProperties.coqShell, cs)
          cur match {
            case Some(y) =>
              if (x == y) {
                end = true
                cur = Some(nextto)
              }
            case None =>
              end = true
              Console.println("how did that happen?, cur is none here :/")
              //how did that happen?
          }
      }
    }
  }

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
      walkAST(cu, doc)

      val selection = editor.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
      val off = selection.getOffset
      val node = findASTNode(cu, off, 0)
      val md = findMethod(node)

      //copy over
      copyProps(method.get, md)
      getProj.program = Some(cu)
      method = Some(md)
      getProj.ASTdirty = false
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
      Some(r.first)
    else
      None
  }

  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.text.source.IAnnotationModelExtension
  def reAnnotate (proc : Boolean, undo : Boolean) : Unit = {
    val m = (method != None)
    Console.println("reannotate called with proc " + proc + " undo " + undo + " method " + m + " editor " + editor)
    //4 cases:
    // #t #f => processing       remove nothing, mark yellow
    // #t #t => problem marker   remove yellow, mark nothing
    // #f #t => real undo        remove green, mark green
    // #f #f => processed!       remove yellow & green, mark green
    if (editor != null && method != None) {
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val proj = getProj
      if (proj.proofShell != None) {
        //we're in proof mode!

        val start = method.get.getStartPosition

        if (!proc && undo) {
          cur match {
            case Some(x) =>
              Console.println("removing coqshell property")
              x.setProperty(EclipseJavaASTProperties.coqShell, null)
            case None =>
          }
          cur = next
          next = None
        }

        Console.println(" reAnn (cur: " + cur + " next: " + next + ") proc " + proc + " undo " + undo)
        if (next != None && !proc && !undo) {
          Console.println("  ass " + cur + " now " + next)
          cur = next
          next = None
        }

        //preserve current shell -- if success!
        if (!proc && !undo)
          cur match {
            case None =>
              Console.println("preserving initial coq shell")
              method.get.setProperty(EclipseJavaASTProperties.coqShell, CoqState.getShell)
            case Some(x) =>
              Console.println("preserving for " + x + " shell " + CoqState.getShell)
              x.setProperty(EclipseJavaASTProperties.coqShell, CoqState.getShell)
          }


        val end =
          cur match {
            case None =>
              val c1 = doc.getLineOfOffset(start)
              doc.getLineOffset(c1 + 1) - 2
            case Some(x) =>
              x.getStartPosition + x.getLength
          }

        val annmodel = prov.getAnnotationModel(editor.getEditorInput)
        annmodel.connect(doc)

        if ((proc && undo) || (!proc && !undo)) {
          processing match {
            case Some(x) =>
              Console.println("removing processing")
              annmodel.removeAnnotation(x)
              Display.getDefault.asyncExec(
                new Runnable() {
                  def run() = { editor.getViewer.invalidateTextPresentation }})
            case None =>
          }
          processing = None
        }

        if ((!proc && undo) || (!proc && !undo)) {
          //re-mark green!
          val p = new Position(start, end - start)
          processed match {
            case Some(x) =>
              val op = annmodel.getPosition(x)
              Console.println("adjusting processed: was " + op.getLength + " now " + p.getLength)
              val tst = ((op.getLength > p.getLength) || (op.getOffset != p.getOffset))
              annmodel.asInstanceOf[IAnnotationModelExtension].modifyAnnotationPosition(x, p)
              if (tst)
                Display.getDefault.asyncExec(
                  new Runnable() {
                    def run() = { editor.getViewer.invalidateTextPresentation }})
            case None =>
              Console.println("new processed annotation")
              val ann = new Annotation("dk.itu.sdg.kopitiam.processed", false, "Proof")
              annmodel.addAnnotation(ann, p)
              processed = Some(ann)
          }
        }

        if (proc && !undo) {
          val pp : Int = processed match {
            case None =>
              start
            case Some(x) =>
              Console.println("found a processed!")
              val p = annmodel.getPosition(x)
              p.getOffset + p.getLength
          }
          val rend : Int =
            next match {
              case None => end
              case Some(x) => x.getStartPosition + x.getLength
            }
          val txt = "dk.itu.sdg.kopitiam.processing"
          val sma = new Annotation(txt, false, "Proof")
          Console.println("new processing annotation (st " + start + " pp " + pp + " end " + end + ")")
          annmodel.addAnnotation(sma, new Position(pp, rend - pp))
          processing match {
            case Some(x) =>
              Console.println("remove processing again")
              annmodel.removeAnnotation(x)
            case None =>
          }
          processing = Some(sma)
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
      //Console.println("searched for comments in: " + con.drop(DocumentState.position - 10).take(20) + " spos: " + (spos - DocumentState.position) + " commentoff " + commentoff + " con at spos: " + con.drop(spos).take(10))
      marker.setAttribute(IMarker.CHAR_START, spos + commentoff)
      marker.setAttribute(IMarker.CHAR_END, epos) //for tha whitespace
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
        Console.println("no content. activeeditor is " + activeEditor)
        if (activeEditor != null)
          _content = Some(activeDocument.get)
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
        if (nextCommand == None && Activator.getDefault.getPreferenceStore.getBoolean("smartcompilation")) {
          //getFullPath is relative to Workspace
          val cwd = resource.getProject.getLocation.toFile
          val nam = resource.getName
          Console.println("no next command in here, starting a ccj! (with " + nam + ")")
          new CoqCompileJob(cwd, nam, false).schedule
        }
        //mutated in nextCommand
        sendlen = 0

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
import akka.actor.Props
class Startup extends IStartup {
  import org.eclipse.core.runtime.Platform

  override def earlyStartup () : Unit = {
    Console.println("earlyStartup called")
    ActionDisabler.disableAll
    DocumentMonitor.init
    CoqTop.init
    CoqTop.consoleprinter = CoqTop.system.actorOf(Props[ConsolePrinter], name = "ConsolePrinter")
    PrintActor.register(DocumentState)
    CoqTop.coqpath = Activator.getDefault.getPreferenceStore.getString("coqpath") + System.getProperty("file.separator")
    ActionDisabler.initializationFinished
  }
}

import akka.actor.{Actor, ActorRef}
class ConsolePrinter extends Actor {
  def receive = {
    case msg : String =>
      EclipseConsole.out.println(msg)
    case _ =>
  }
}

import org.eclipse.core.runtime.jobs.Job
import java.io.File
class CoqCompileJob (path : File, name : String, requiressuccess : Boolean) extends Job (name : String) {

  import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status}
  import java.io.File
  override def run (mon : IProgressMonitor) : IStatus = {
    Console.println("hello, world!, " + name)
    if (EclipseConsole.out == null)
      EclipseConsole.initConsole
    val la = if (CoqTop.isWin) ".exe" else ""
    val coqc = CoqTop.coqpath + "coqc" + la
    //what about dependencies?? <- need Add LoadPath explicitly in .v!
    if (new File(coqc).exists) {
      val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
      val lp = new File(loadp).exists
      val cmdarr =
        if (lp)
          Array(coqc, "-I", loadp, name)
        else
          Array(coqc, name)
      val coqcp = Runtime.getRuntime.exec(cmdarr, null, path)
      val ou = coqcp.getInputStream
      val err = coqcp.getErrorStream
      val bs = new BusyStreamReader(ou)
      val bs2 = new BusyStreamReader(err)
      bs.addActor(CoqTop.consoleprinter)
      bs2.addActor(CoqTop.consoleprinter)
      new Thread(bs).start
      new Thread(bs2).start
      coqcp.waitFor
      if (requiressuccess)
        if (coqcp.exitValue != 0) {
          Console.println("errrrrrrrrror")
          CoqCommands.empty
        } else
          CoqCommands.step
      bs.act = false
      bs2.act = false
      Console.println("done")
    }
    Status.OK_STATUS
  }
}
