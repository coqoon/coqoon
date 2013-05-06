/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.jdt.core.dom.Statement
import org.eclipse.ui.texteditor.ITextEditor

case class JavaStep(
    node : Statement,
    text : String) extends CoqCommand {
  override def run(coqTop : CoqTopIdeSlave_v20120710) =
    coqTop.interp(false, false, text)
}

abstract class JavaEditorHandler extends EditorHandler {
  override def editor : ITextEditor = {
    if (super.editor.isInstanceOf[ITextEditor]) {
      super.editor.asInstanceOf[ITextEditor]
    } else null
  }
  
  override def calculateEnabled : Boolean = {
    if (editor == null)
      return false
    val state = getState
    if (state == null || state.busy)
      return false
    return true
  }
  
  protected def getState = JavaEditorState.requireStateFor(editor)
}

import org.eclipse.core.commands.ExecutionEvent

class VerifyMethodHandler extends JavaEditorHandler
    with EclipseJavaHelper with CoreJavaChecker {
  import org.eclipse.jface.text.ITextSelection
  override def execute (ev : ExecutionEvent) : Object = {
    if (isEnabled()) {
      //plan:
      // a: get project
      val jes = getState
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val bla = getRoot(editor.getEditorInput)
      val cu = getCompilationUnit(bla)
      jes.setCompilationUnit(Some(cu))
      jes.method.map(_ => { jes.setUnderway(None); jes.setMethod(None) })
      // a': CoreJava checking!
      if (checkAST(jes, cu, doc)) { //no errors!
        // b: if outdated coqString: translate -- need to verify outdated...
        // c: find method and statement we want to prove
        if (walkAST(jes, cu, doc)) { //no errors!
          val selection = editor.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
          val off = selection.getOffset
          val node = findASTNode(cu, off, 0)
          val md = findMethod(node)
          md match {
            case None => EclipseBoilerPlate.warnUser("Cursor not inside of method", "Please put the cursor inside of the method to verify")
            case Some(x) =>
              if (jes.completedMethods.contains(x))
                EclipseBoilerPlate.warnUser("Already proven", "Sorry, this method was already proven")
              else {
                jes.setMethod(Some(x))
                new JavaProofInitialisationJob(jes).schedule
                //proj.proveMethod(x)
              }
          }
        }
      }
    }
    null
  }
}

class JavaStepForwardHandler extends JavaEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val jes = getState
      JavaStepForwardHandler.collectProofScript(jes, false) match {
        case a : List[(Statement, String)] if a.size == 1 =>
          jes.setUnderway(Some(a.last._1))
          scheduleJob(
              new JavaStepJob(a.map(b => new JavaStep(b._1, b._2)), jes))
        case _ =>
      }
    }
    null
  }
}
protected object JavaStepForwardHandler extends JavaASTUtils {
  def collectProofScript(
      jes : JavaEditorState, multiple : Boolean) :
      List[(Statement, String)] = {
    var captureNext : Boolean = (jes.complete == None)

    def print(x: Statement): Option[(Statement, String)] =
      if (captureNext) {
        val ps = printProofScript(jes.getIDocument, x)
        ps match {
          case None => None
          case Some(ps) =>
            Some((x, ps))
        }
      } else {
        if (jes.complete.get == x)
          captureNext = true
        None
      }

    traverseAST(jes.method.get, true, !multiple, print)
  }
}

class JavaStepAllHandler extends JavaEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val jes = getState
      JavaStepForwardHandler.collectProofScript(jes, true) match {
        case a : List[(Statement, String)] if a.size > 0 =>
          jes.setUnderway(Some(a.last._1))
          scheduleJob(
              new JavaStepJob(a.map(b => new JavaStep(b._1, b._2)), jes))
        case _ =>
      }
    }
    null
  }
}