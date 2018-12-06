package dk.itu.coqoon.ui.editors.coqidetop

import dk.itu.coqoon.ui.editors.{BaseCoqEditor, CoqGoalsContainer}
import dk.itu.coqoon.ui.editors.pide.{QueryHost, QueryListener}
import dk.itu.coqoon.ui.utilities.{UIUtils, EclipseConsole}
import dk.itu.coqoon.core.model.{ICoqModel, ICoqProject, ICoqScriptSentence}
import dk.itu.coqoon.core.coqtop.coqidetop.{
  Feedback, Interface, CoqIdeTopFeedbackListener}
import dk.itu.coqoon.core.utilities.SupersedableTask

class CoqIdeTopEditor extends BaseCoqEditor
    with CoqGoalsContainer with QueryHost[ICoqScriptSentence] {
  import dk.itu.coqoon.core.coqtop.coqidetop.{
    CoqIdeTop_v20170413, StateTracker}
  private lazy val st = {
    val st = new StateTracker(CoqIdeTop_v20170413())
    st.addListener(FeedbackReconciler)
    st
  }

  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.text.source.IVerticalRuler
  override protected def createSourceViewer(
      parent : Composite, ruler : IVerticalRuler, styles : Int) = {
    val viewer = super.createSourceViewer(parent, ruler, styles)
    viewer.getTextWidget.addCaretListener(DocumentCaretListener)

    viewer
  }

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jface.text.source.AnnotationModel
  protected def getAnnotationModel() = Option(getDocumentProvider).flatMap(
      p => Option(p.getAnnotationModel(getEditorInput)).flatMap(
          TryCast[AnnotationModel]))

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    val task = new SupersedableTask(200)
    override def caretMoved(ev : CaretEvent) =
      task schedule {
        caretPing
      }
  }

  private def caretPing() =
    UIUtils.asyncExec {
      Option(getViewer).map(_.getTextWidget).filter(
          text => !text.isDisposed).map(_.getCaretOffset).foreach(caret_ => {
        var caret = caret_
        var sentence : Option[ICoqScriptSentence] = None
        while (sentence.isEmpty && caret >= 0) {
          sentence = getWorkingCopy.get.get.getSentenceAt(caret)
          if (!sentence.exists(st.sentenceKnown))
            caret = sentence.map(_.getOffset - 1).getOrElse(caret - 1)
        }
        sentence.flatMap(st.getStatus) foreach {
          case Interface.Fail((_, loc, msg)) =>
            EclipseConsole.err.println(msg)
          case _ =>
        }
        setGoals(sentence.flatMap(st.getGoals))
        sentence.flatMap(st.getFeedback).toSeq.flatten.foreach {
          case Feedback(_, _, _, Feedback.Message((level, None, text))) =>
            EclipseConsole.out.println(text)
          case _ =>
        }
      })
    }

  import dk.itu.coqoon.core.coqtop.CoqSentence
  import dk.itu.coqoon.core.utilities.TotalReader
  import org.eclipse.ui.{IEditorInput, IFileEditorInput}
  override def doSetInput(input : IEditorInput) = {
    super.doSetInput(input)

    val fi = TryCast[IFileEditorInput](getEditorInput)
    val initialisationBlockContent =
      fi.flatMap(input => ICoqModel.getInstance.toCoqElement(
          input.getFile.getProject)) match {
        case Some(cp : ICoqProject) =>
          cp.getLoadPath.flatMap(_.asCommands).mkString("", "\n", "\n")
        case _ =>
          EclipseConsole.err.println(
s"""The Coq file ${fi.map(_.getName).getOrElse("(unknown")} is not part of a Coqoon project.
(Using Coqoon in this way is not recommended; Coq files should generally be
kept in Coqoon projects.)
A simple Coq session has been created for it with an empty load path.
Some Coqoon features may not work in this session.""")
          ""
      }

    st.attach(getWorkingCopy().get.get, initialisationBlockContent)
  }

  import org.eclipse.core.resources.IFile
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)

  override def findActiveCommand() =
    getWorkingCopy.asOption.map(_.get).flatMap(
        _.getSentenceAt(getViewer.getTextWidget.getCaretOffset))
  override def runQuery(s : ICoqScriptSentence,
      queryContent : String, responseListener : QueryListener) =
    responseListener.onQueryResult(st.query(s, queryContent))

  private object FeedbackReconciler extends SupersedableTask(50)
      with CoqIdeTopFeedbackListener {
    override def onFeedback(f : Feedback) =
      schedule {
        UIUtils.asyncExec(processFeedback)
      }
  }
  private def processFeedback() = {
    for (Some(wc) <- getWorkingCopy.asOption;
         s <- wc.getSentences) {
      var state : Option[Feedback.FeedbackContent] = None
      st.getFeedback(s).foreach(_.map(_.content).foreach(fc =>
          fc match {
            case Feedback.ProcessingIn(_) | Feedback.Processed |
                 Feedback.InProgress(_) | Feedback.Incomplete |
                 Feedback.Complete =>
              state = Some(fc)
            case _ =>
          }))
    }
    caretPing
  }
}

import dk.itu.coqoon.ui.editors.pide.QueryHandler
class CoqIdeTopQueryHandler extends QueryHandler[ICoqScriptSentence]