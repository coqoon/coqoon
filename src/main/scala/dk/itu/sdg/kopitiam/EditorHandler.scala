package dk.itu.sdg.kopitiam

import org.eclipse.ui.{ISources, IEditorPart}
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.expressions.IEvaluationContext

abstract class EditorHandler extends AbstractHandler {
  private var editorV : IEditorPart = null
  protected def editor = editorV
  
  override def setEnabled(evaluationContext : Object) = {
    val activeEditor = if (evaluationContext != null) {
      evaluationContext.asInstanceOf[IEvaluationContext].getVariable(
          ISources.ACTIVE_EDITOR_NAME)
    } else org.eclipse.ui.PlatformUI.getWorkbench().
        getActiveWorkbenchWindow().getActivePage().getActiveEditor()
    if (activeEditor != null && activeEditor.isInstanceOf[IEditorPart]) {
      editorV = activeEditor.asInstanceOf[IEditorPart]
      setBaseEnabled(calculateEnabled)
    } else setBaseEnabled(false)
  }
  
  protected def getCoqTopContainer = {
    if (editor != null) {
      val ad = editor.getAdapter(classOf[CoqTopContainer])
      if (ad != null && ad.isInstanceOf[CoqTopContainer]) {
        ad.asInstanceOf[CoqTopContainer]
      } else null
    } else null
  }
  
  import org.eclipse.core.runtime.jobs.Job
  protected def scheduleJob(j : Job) = {
    getCoqTopContainer.setBusy(true)
    j.schedule
  }
  
  def calculateEnabled : Boolean = true
}