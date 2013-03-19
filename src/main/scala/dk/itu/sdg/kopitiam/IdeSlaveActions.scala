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
    undo : Pair[Int, Option[String]]) // (rewind-steps, undo-command)

import scala.collection.mutable.Stack
trait Editor extends org.eclipse.ui.IEditorPart {
  def steps : Stack[CoqStep]
  def coqTop : CoqTopIdeSlave_v20120710
  
  def document : String
  def cursorPosition : Int
  
  def underway : Int
  def setUnderway(offset : Int)
  
  def completed : Int
  def setCompleted(offset : Int)
  
  def goals : CoqTypes.goals
  def setGoals(goals : CoqTypes.goals)
}

class NewEditorAction(
    protected val editor : Editor) extends org.eclipse.jface.action.Action

object NewEditorAction {
  def makeStep(doc : String, offset : Int) : Option[CoqStep] = {
    Some(CoqStep(offset, "", (1, None)))
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
}
    
class NewStepOneForwardAction(editor : Editor)
    extends NewEditorAction(editor) {
  override def run = {
    if (isEnabled()) {
      NewEditorAction.makeStep(editor.document, editor.underway) match {
        case Some(step) =>
          // We're running in the UI thread, so always move the underway marker
          editor.setUnderway(step.offset + step.text.length())
          new StepOneForwardJob(editor, step).schedule()
        case _ =>
      }
    }
  }
}

class NewStepToCursorAction(editor : Editor) extends NewEditorAction(editor) {
  override def run = {
    if (isEnabled()) {
      val underwayPos = editor.underway
      val cursorPos = editor.cursorPosition
      if (cursorPos > underwayPos) { // Forwards!
        val steps = NewEditorAction.makeSteps(
          editor.document, editor.underway, editor.cursorPosition)
        if (steps.length > 0) {
          editor.setUnderway(steps.last.offset + steps.last.text.length)
          for (val i <- steps)
            new StepOneForwardJob(editor, i).schedule()
        }
      } else if (cursorPos < underwayPos) { // Backwards!
        
      }
    }
  }
}

class NewRestartCoqAction(editor : Editor) extends NewEditorAction(editor) {
  override def run = {
    if (isEnabled())
      new RestartCoqJob(editor).schedule()
  }
}
