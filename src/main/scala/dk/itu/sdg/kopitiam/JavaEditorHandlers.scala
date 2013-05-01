/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.texteditor.ITextEditor

abstract class JavaEditorHandler extends EditorHandler {
  override def editor : ITextEditor = {
    if (super.editor.isInstanceOf[ITextEditor]) {
      super.editor.asInstanceOf[ITextEditor]
    } else null
  }
  
  override def calculateEnabled = (editor != null)
  
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

class JavaStepForwardHandler
    extends JavaEditorHandler with EclipseJavaHelper with JavaASTUtils {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val jes = getState

      var captureNext: Boolean = (jes.complete == None)

      import org.eclipse.jdt.core.dom.Statement
      
      val print: Statement => Option[String] = x =>
        if (captureNext) {
          val ps = printProofScript(jes.getIDocument, x)
          ps match {
            case None => None
            case Some(ps) =>
              jes.setUnderway(Some(x))
              Some(ps)
          }
        } else {
          if (jes.complete.get == x)
            captureNext = true
          None
        }

      traverseAST(jes.method.get, true, true, print) match {
        case a : List[String] if a.size == 1 =>
          jes.coqTop.interp(false, false, a.head) match {
            case CoqTypes.Good(msg) =>
              jes.setComplete(jes.underway)
            case CoqTypes.Fail((position, msg)) =>
              jes.setUnderway(jes.complete)
            case CoqTypes.Unsafe(msg) =>
              println("I have no idea " + msg)
          }
        case _ => None
      }
      
      jes.coqTop.goals match {
        case CoqTypes.Good(goals) =>
          jes.setGoals(goals)
          goals match {
            case Some(goals)
                if !(goals.fg_goals.isEmpty && goals.bg_goals.isEmpty) =>
            case _ =>
              jes.coqTop.interp(false, false, "Qed.") match {
                case CoqTypes.Good(s) =>
                  val method = jes.method.get
                  jes.completedMethods :+= method
                case _ =>
              }
              /* Whether we succeeded or not, there's nothing more to do */
              jes.setMethod(None)
              jes.setUnderway(None)
              jes.annotateCompletedMethods
          }
        case _ => jes.setGoals(None)
      }
    }
    null
  }
}
