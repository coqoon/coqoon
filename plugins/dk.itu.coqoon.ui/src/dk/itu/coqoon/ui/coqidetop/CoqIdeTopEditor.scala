package dk.itu.coqoon.ui.coqidetop

import dk.itu.coqoon.ui.{BaseCoqEditor, CoqGoalsContainer}
import dk.itu.coqoon.ui.utilities.{UIUtils, EclipseConsole}
import dk.itu.coqoon.core.model.ICoqScriptSentence
import dk.itu.coqoon.core.coqtop.coqidetop.{Feedback, Interface}
import dk.itu.coqoon.core.utilities.SupersedableTask

class CoqIdeTopEditor
    extends BaseCoqEditor with CoqGoalsContainer {
  import dk.itu.coqoon.core.coqtop.coqidetop.{
    CoqIdeTop_v20170413, StateTracker}
  private val st = new StateTracker(CoqIdeTop_v20170413())

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
          case w =>
            println(w)
        }
      })
    }

  import dk.itu.coqoon.core.coqtop.CoqSentence
  import dk.itu.coqoon.core.utilities.TotalReader
  import org.eclipse.ui.{IEditorInput, IFileEditorInput}
  override def doSetInput(input : IEditorInput) = {
    super.doSetInput(input)
    st.attach(getWorkingCopy().get.get)
  }

  import org.eclipse.core.resources.IFile
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)

  import dk.itu.coqoon.ui.EventReconciler._
  def reconcileEvents(events : List[DecoratedEvent]) = {
    DocumentCaretListener.task.schedule {
      caretPing
    }
  }
  getReconciler.addHandler(reconcileEvents)
}