package dk.itu.coqoon.core.coqtop.coqidetop

import scala.collection.DefaultMap
import scala.collection.mutable.{Buffer => MBuffer}

import dk.itu.coqoon.core.model.{ICoqVernacFile}
import dk.itu.coqoon.core.model.{ICoqScriptGroup, ICoqScriptSentence}
import dk.itu.coqoon.core.model.CoqElementChangeListener
import dk.itu.coqoon.core.model.{CoqElementEvent, CoqFileContentChangedEvent}

class StateTracker(
    ct : CoqIdeTop_v20170413) {
  import StateTracker._

  private var file : Option[ICoqVernacFile] = None
  private var stateIds : Seq[(SentenceID, Interface.state_id)] = Seq()

  private var goals : Map[Interface.state_id, Interface.goals] = Map()
  private var status :
      Map[Interface.state_id, Interface.value[Interface.status]] = Map()
  private var feedback : Map[Interface.state_id, MBuffer[Feedback]] = Map()

  def sentenceKnown(s : ICoqScriptSentence) = (getStateID(s) != None)

  private def getStateID(
      s : ICoqScriptSentence) : Option[Interface.state_id] = {
    val sentenceID = makeSentenceID(s)
    stateIds.find(_._1 == sentenceID).map(_._2)
  }

  def getGoals(s : ICoqScriptSentence) = getStateID(s).flatMap(goals.get)
  def getStatus(s : ICoqScriptSentence) = getStateID(s).flatMap(status.get)
  def getFeedback(s : ICoqScriptSentence) = getStateID(s).flatMap(feedback.get)

  private def removeData(sids : Seq[Interface.state_id]) =
    sids.foreach(sid => {
      goals -= sid
      status -= sid
      feedback -= sid
    })

  private def getHead() : Interface.state_id =
    stateIds.lastOption.map(_._2).getOrElse(1)

  private object ChangeListener
      extends CoqElementChangeListener with CoqIdeTopFeedbackListener {
    override def coqElementChanged(ev : CoqElementEvent) =
      ev match {
        case CoqFileContentChangedEvent(f) if file.contains(f) =>
          stateIds = Seq()
          goals = Map()
          status = Map()
          feedback = Map()
          ct.editAt(1)
          f.accept {
            case s : ICoqScriptSentence =>
              submit(s)
              false
            case _ : ICoqScriptGroup | _ : ICoqVernacFile =>
              true
          }
        case _ =>
    }
    override def onFeedback(f : Feedback) = {
      /* Feedback might arrive before we've processed the return value of
       * add(), so instead of going through stateIds to work out which command
       * this message is associated with, just stash it away immediately */
      if (!feedback.contains(f.state))
        feedback += (f.state -> MBuffer())
      feedback.get(f.state).get += f
    }
  }
  ct.addListener(ChangeListener)

  def submit(s : ICoqScriptSentence) =
    if (!s.isSynthetic) {
      ct.add(getHead, s.getText, true) match {
        case Interface.Good((sid, (Left(()), msg))) =>
          stateIds :+= (makeSentenceID(s) -> sid)
          val st = ct.status(false)
          status += (sid -> st)
          st match {
            case Interface.Good(Interface.status(_, Some(_), _, _)) =>
              ct.goal() match {
                case Interface.Good(Some(gs)) =>
                  goals += (sid -> gs)
                case _ =>
              }
            case _ =>
          }
      }
    }

  def attach(f : ICoqVernacFile) = {
    file.foreach(_ => detach())
    file = Some(f)
    f.getModel.addListener(ChangeListener)
    f.accept {
      case s : ICoqScriptSentence =>
        submit(s)
        false
      case _ : ICoqScriptGroup | _ : ICoqVernacFile =>
        true
    }
  }
  def detach() = file.foreach(f => {
    f.getModel.removeListener(ChangeListener)
    file = None
    stateIds = Seq()
    goals = Map()
    status = Map()
    feedback = Map()
  })
}
object StateTracker {
  type SentenceID = (Int, Int)
  private def makeSentenceID(s : ICoqScriptSentence) : SentenceID =
    (s.getOffset, s.getText.hashCode)
}