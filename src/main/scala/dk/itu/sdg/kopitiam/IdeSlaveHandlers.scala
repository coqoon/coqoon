/* IdeSlaveActions.scala
 * Eclipse Action wrappers for coqtop functionality
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

case class CoqStep(
    offset : Int,
    text : String,
    synthetic : Boolean) // true iff this step doesn't need to be rewound

trait CoqTopContainer {
  def coqTop : CoqTopIdeSlave_v20120710
  
  private var goals_ : Option[CoqTypes.goals] = None
  def goals = goals_
  def setGoals(g : Option[CoqTypes.goals]) = {
    goals_ = g
    fireChange(CoqTopContainer.PROPERTY_GOALS)
  }
  
  import org.eclipse.ui.IPropertyListener
  
  private var listeners = Set[IPropertyListener]()
  def addListener(l : IPropertyListener) = (listeners += l)
  def removeListener(l : IPropertyListener) = (listeners -= l)
  def fireChange(propertyID : Int) = listeners.map {
    _.propertyChanged(this, propertyID)
  }
}
object CoqTopContainer {
  final val PROPERTY_GOALS = 1234567;
}
    
import scala.collection.mutable.Stack
trait Editor extends CoqTopContainer with org.eclipse.ui.IEditorPart {
  def steps : Stack[CoqStep]
  
  def document : String
  def cursorPosition : Int
  
  def underway : Int
  def setUnderway(offset : Int)
  
  def completed : Int
  def setCompleted(offset : Int)
  
  var enabled_ : Boolean = true
  
  def enabled = enabled_
  def preExecuteJob = (enabled_ = false)
  def postExecuteJob = (enabled_ = true)
}

import org.eclipse.ui.ISources
import org.eclipse.core.commands.{AbstractHandler,ExecutionEvent}
import org.eclipse.core.expressions.IEvaluationContext

abstract class EditorHandler extends AbstractHandler {
  protected var editor : Editor = null
  
  override def setEnabled(evaluationContext : Object) = {
    val activeEditor = if (evaluationContext != null) {
      evaluationContext.asInstanceOf[IEvaluationContext].getVariable(
          ISources.ACTIVE_EDITOR_NAME)
    } else org.eclipse.ui.PlatformUI.getWorkbench().
        getActiveWorkbenchWindow().getActivePage().getActiveEditor()
    if (activeEditor != null && activeEditor.isInstanceOf[CoqEditor]) {
      editor = activeEditor.asInstanceOf[CoqEditor]
      setBaseEnabled(editor.enabled && calculateEnabled)
    } else setBaseEnabled(false)
  }
  
  def calculateEnabled : Boolean = true
}

object EditorHandler {
  def makeStep(doc : String, offset : Int) : Option[CoqStep] = {
    val length = CoqTop.findNextCommand(doc.substring(offset))
    if (length != -1) {
      Some(CoqStep(offset, doc.substring(offset, offset + length), false))
    } else None
  }
  
  def makeSteps(
      doc : String, from : Int, to : Int) : List[CoqStep] = {
    val steps = List.newBuilder[CoqStep]
    var offset = from
    while (offset <= to) {
      makeStep(doc, offset) match {
        case Some(step) =>
          offset = step.offset + step.text.length()
          if (offset <= to)
            steps += step
        case _ => offset = Int.MaxValue
      }
    }
    steps.result
  }
  
  def getStepBackPair(
      editor : Editor,
      f : Stack[CoqStep] => Int) : (Int, Option[CoqStep]) = {
    var count : Int = 0
    var mostRecent : Option[CoqStep] = None
    editor.steps.synchronized {
      count = f(editor.steps)
      if (count > 0 && editor.steps.length - count > 0)
        mostRecent = Some(editor.steps(count))
    }
    (count, mostRecent)
  }
  
  def doStepBack(editor : Editor, f : Stack[CoqStep] => Int) = {
    val p = getStepBackPair(editor, f)
    if (p._1 > 0) {
      editor.setUnderway(p._2 match {
        case None => 0
        case Some(x) => x.offset + x.text.length
      })
      new StepBackJob(editor, p._1).schedule()
    }
  }
}
    
class StepForwardHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      EditorHandler.makeStep(editor.document, editor.underway) match {
        case Some(step) =>
          // We're running in the UI thread, so always move the underway marker
          editor.setUnderway(step.offset + step.text.length())
          new StepForwardJob(editor, List(step)).schedule()
        case _ =>
      }
    }
    null
  }
}

class StepAllHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val doc = editor.document
      val steps = EditorHandler.makeSteps(doc, editor.underway, doc.length)
      if (steps.length > 0) {
        editor.setUnderway(steps.last.offset + steps.last.text.length)
        new StepForwardJob(editor, steps).schedule()
      }
    }
    null
  }
}

class StepToCursorHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val underwayPos = editor.underway
      val cursorPos = editor.cursorPosition
      if (cursorPos > underwayPos) { // Forwards!
        val steps = EditorHandler.makeSteps(
          editor.document, editor.underway, editor.cursorPosition)
        if (steps.length > 0) {
          editor.setUnderway(steps.last.offset + steps.last.text.length)
          new StepForwardJob(editor, steps).schedule()
        }
      } else if (cursorPos < underwayPos) { // Backwards!
        EditorHandler.doStepBack(editor,
            _.prefixLength(a => (cursorPos < (a.offset + a.text.length))))
      }
    }
    null
  }
}

class StepBackHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      EditorHandler.doStepBack(editor, a => if (a.length > 0) 1 else 0)
    null
  }
  
  override def calculateEnabled = (editor.steps.length > 0)
}

class RetractAllHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      EditorHandler.doStepBack(editor, _.length)
    null
  }
  
  override def calculateEnabled = (editor.steps.length > 0)
}

class RestartCoqHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      new RestartCoqJob(editor).schedule()
    null
  }
}

class CompileCoqHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    import org.eclipse.ui.IFileEditorInput
    if (isEnabled()) {
      val f = editor.getEditorInput().asInstanceOf[IFileEditorInput].getFile
      new CoqCompileJob(f).schedule()
    }
    null
  }
}