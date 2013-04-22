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
  private var javaSource : Option[IDocument] = None
  private var coqModel : Option[IDocument] = None
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

  //XXX: this was broken, but didn't hurt too much -- maybe we don't need this anymore?
  def setDocument (doc : IDocument, name : String) : Unit = {
    if (name.equals(basename + ".java")) {
      assert(javaSource == None)
      javaSource = Some(doc)
    } else if (name.equals(basename + "_model.v")) {
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
    /*CoqCommands.doLater(() => {
      if (DocumentState.activeEditor != null) {
        DocumentState.resetState
        DocumentState.activeEditor.addAnnotations(0, 0)
        DocumentState.activeEditor.invalidate
        DocumentState.setBusy
        val shell = CoqTop.dummy
      }
      (CoqCommands.step)
    })*/
    /*CoqCommands.doLater*/(() => {
      proofShell match {
        case None =>
          Console.println("sending defs + spec")
          //DocumentState._content = getCoqString
          //CoqStepAllAction.doitH
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
          Console.println("have a PS: " + sh.globalStep + " < " + CoqTop.dummy.globalStep)
          if (sh.globalStep < CoqTop.dummy.globalStep) {
            JavaPosition.cur = None
            JavaPosition.next = None
            JavaPosition.emptyCoqShells
            JavaPosition.method = None
            Console.println("backtracking to shell " + sh)
            /*CoqTop.writeToCoq*/("Backtrack " + sh.globalStep + " 0 " + CoqTop.dummy.context.length + ".")
          } else
            (/*CoqCommands.step*/)
      }
    })
    /*CoqCommands.doLater*/(() => {
      if (proofShell == None) {
        Console.println("preserving proof shell: " + CoqTop.dummy)
        proofShell = Some(CoqTop.dummy)
      }
      if (JavaPosition.method == None) {
        Console.println("assigning method to JP ")
        //story so far: model is now updated, java might be newly generated!
        JavaPosition.method = Some(meth)
        val prf = meth.getProperty(EclipseJavaASTProperties.coqProof)
        assert(prf != null)
        val p = prf.asInstanceOf[String]
        Console.println("p is " + p)
        //DocumentState._content = Some(DocumentState._content.getOrElse("") + p)
        //CoqStepAllAction.doitH
      } else
        (/*CoqCommands.step*/)
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

object JavaPosition extends EclipseJavaHelper with JavaASTUtils {
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

  def unmarkProof (m : MethodDeclaration) : Unit = {
    val nam = m.getName.getIdentifier
    if (proofmarkers.contains(nam)) {
      proofmarkers(nam).delete
      proofmarkers -= nam
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
  def removeMarkers () : Unit = {
    if (editor != null) {
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
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = { if (editor != null) editor.getViewer.invalidateTextPresentation }})
    }
  }

  def retract () : Unit = {
    val mn = (method != None)
    Console.println("retracting with " + editor + " and method? " + mn)
    if (editor != null && method != None) {
      Console.println("hello my friend, NONONONO")
      removeMarkers
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
        findMethod(n) match {
          case None =>
          case Some(x) =>
            Console.println("Is the method the same? " + (x == method.get))
            if (x == method.get) {
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

              val r = traverseAST(x, false, true, cb)
              assert(r.size == 1)
              next = Some(r.first)
            }
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
      Some(r.first)
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
              method.get.setProperty(EclipseJavaASTProperties.coqShell, CoqTop.dummy)
            case Some(x) =>
              Console.println("preserving for " + x + " shell " + CoqTop.dummy)
              x.setProperty(EclipseJavaASTProperties.coqShell, CoqTop.dummy)
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

import org.eclipse.ui.part.ViewPart
import org.eclipse.ui.{IPropertyListener, IPartListener2}

class GoalViewer extends ViewPart with IPropertyListener with IPartListener2 {
  import org.eclipse.swt.widgets.{Composite,Text}
  import org.eclipse.swt.SWT
  import org.eclipse.swt.layout.{FormData,FormLayout,FormAttachment}
  import org.eclipse.swt.graphics.{Color,RGB,Rectangle}
  import org.eclipse.swt.widgets.{Display,Sash,Listener,Event,TabFolder,TabItem}
//  import org.eclipse.swt.custom.{CTabFolder,CTabItem}

  import org.eclipse.ui.texteditor.ITextEditor
  
  override def propertyChanged (source : Object, propID : Int) = {
    if (source.isInstanceOf[CoqTopContainer] &&
        propID == CoqTopContainer.PROPERTY_GOALS)
      writeGoal(source.asInstanceOf[CoqTopContainer].goals)
  }

  import org.eclipse.ui.IViewSite
  override def init (site : IViewSite) = {
    super.init(site)
    site.getWorkbenchWindow().getPartService().addPartListener(this)
  }
  
  override def dispose = {
    getSite.getWorkbenchWindow().getPartService().removePartListener(this)
    super.dispose
  }
  
  import org.eclipse.ui.IEditorPart
  private var activeContainer : Option[CoqTopContainer] = None
  private def setActiveContainer (e : IEditorPart) = {
    println("" + this + ".setActiveEditor(" + e + ")")
    activeContainer match {
      case Some(ed) => ed.removeListener(this)
      case None =>
    }
    activeContainer = if (e.isInstanceOf[CoqTopContainer]) {
      Some(e.asInstanceOf[CoqTopContainer])
    } else if (e.isInstanceOf[ITextEditor]) {
      Some(JavaEditorState.requireStateFor(e))
    } else None
    activeContainer match {
      case Some(c) =>
        c.addListener(this)
        writeGoal(c.goals)
      case None =>
        writeGoal(None)
    }
  }
  
  import org.eclipse.ui.IWorkbenchPartReference
  override def partOpened (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (p == this)
      setActiveContainer(
          getSite.getWorkbenchWindow().getActivePage().getActiveEditor())
  }
  
  override def partClosed (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (this == p || Some(p) == activeContainer)
      setActiveContainer(null)
  }
  
  override def partActivated (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (p.isInstanceOf[IEditorPart] && Some(p) != activeContainer)
      setActiveContainer(p.asInstanceOf[IEditorPart])
  }

  override def partDeactivated (ref : IWorkbenchPartReference) = ()

  override def partBroughtToTop (part : IWorkbenchPartReference) : Unit = { }
  override def partHidden (part : IWorkbenchPartReference) : Unit = { }
  override def partInputChanged (part : IWorkbenchPartReference) : Unit = { }
  override def partVisible (part : IWorkbenchPartReference) : Unit = { }

  
  var goals : TabFolder = null
  def subgoals = goals.getItems()
  var comp : Composite = null

  import org.eclipse.swt.layout.FillLayout
  
  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new FillLayout())

    goals = new TabFolder(comp, SWT.NONE)
  }

  /*class SashListener(sash : Sash, comp : Composite, limit : Int)
      extends Listener {
    override def handleEvent (e : Event) = {
      val sashRect : Rectangle = sash.getBounds()
      val shellRect : Rectangle = comp.getClientArea()
      val top = shellRect.height - sashRect.height - limit
      e.y = scala.math.max(scala.math.min(e.y, top), limit)
      if (e.y != sashRect.y)  {
        sashData.top = new FormAttachment (0, e.y)
        comp.layout()
      }
    }
  }*/
  
  private def writeGoal (coqGoals : Option[CoqTypes.goals]) : Unit = {
    if (!comp.isDisposed) {
      val goalData = coqGoals match {
        case None => List.empty
        case Some(x) => x.fg_goals
      }
      if (subgoals.length < goalData.length) {
        while (subgoals.length != goalData.length) {
          val ti = new TabItem(goals, SWT.NONE)
          val area = new Composite(goals, SWT.NONE)
          area.setLayout(new FillLayout(SWT.VERTICAL))
          ti.setControl(area)
          
          // val sash = new Sash(area, SWT.HORIZONTAL)
          new Text(area,
              SWT.BORDER | SWT.READ_ONLY | SWT.MULTI |
              SWT.H_SCROLL | SWT.V_SCROLL)
          new Text(area,
              SWT.BORDER | SWT.READ_ONLY | SWT.MULTI |
              SWT.H_SCROLL | SWT.V_SCROLL)
          ti.setText(subgoals.length.toString)
        }
      } else {
        while (subgoals.length != goalData.length)
          subgoals.last.dispose()
      }
      goals.pack
      goalData.zip(subgoals).foreach(_ match {
        case (goal, control) =>
          val comp = control.getControl().asInstanceOf[Composite]
          comp.getChildren()(0).asInstanceOf[Text].setText(goal.goal_hyp.mkString("\n"))
          comp.getChildren()(1).asInstanceOf[Text].setText(goal.goal_ccl)
      })
      comp.layout
    }
  }

  def setFocus() : Unit = {
  //  viewer.getControl.setFocus
  }
}

import org.eclipse.ui.IStartup
class Startup extends IStartup {
  import org.eclipse.core.runtime.Platform
  
  override def earlyStartup () : Unit = {
    Console.println("earlyStartup called")
    DocumentMonitor.init
  }
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