package dk.itu.coqoon.core.coqtop.coqidetop

import scala.collection.DefaultMap
import scala.collection.mutable.{Buffer => MBuffer}

import dk.itu.coqoon.core.model.CoqEnforcement
import dk.itu.coqoon.core.model.ICoqVernacFile
import dk.itu.coqoon.core.model.{ICoqScriptGroup, ICoqScriptSentence}
import dk.itu.coqoon.core.model.CoqElementChangeListener
import dk.itu.coqoon.core.model.{CoqElementEvent, CoqFileContentChangedEvent}
import dk.itu.coqoon.core.coqtop.CoqSentence

class StateTracker(
    ct : CoqIdeTop_v20170413) {
  import StateTracker._

  private var file : Option[ICoqVernacFile] = None
  private var initialisationBlock : Option[String] = None
  private var stateIds : Seq[(SentenceID, Interface.state_id)] = Seq()

  private var goals : Map[Interface.state_id, Interface.goals] = Map()
  private var status :
      Map[Interface.state_id, Interface.value[Interface.status]] = Map()
  private var feedback : Map[Interface.state_id, MBuffer[Feedback]] = Map()

  private def sendInitialisationBlock() =
    initialisationBlock.foreach(ib => {
      CoqSentence.getNextSentences(ib, 0, ib.length).foreach(
          s => submit(Left(s)))
    })

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
        case CoqFileContentChangedEvent(
            f : ICoqVernacFile) if file.contains(f) =>
          val zippedIDs = f.getSentences.map(
              s => Some(makeSentenceID(s))).zipAll(
                  stateIds.map(s => Some(s._1)), None, None)
          val divergence = zippedIDs.indexWhere {
            case (a, b) if a != b =>
              true
            case (a, b) =>
              false
          }
          if (divergence != -1) {
            removeData(stateIds.drop(divergence).map(_._2))
            stateIds = stateIds.take(divergence)
            ct.editAt(getHead)
            if (getHead == 1)
              sendInitialisationBlock
            submitAll(f.getSentences.drop(divergence))
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

  def submitAll(sentences_ : Seq[ICoqScriptSentence]) = {
    var sentences = sentences_
    var continuing = true
    while (continuing && !sentences.isEmpty) {
      continuing = submit(Right(sentences.head))
      sentences = sentences.tail
    }
  }
  def submit(
      s : Either[CoqSentence.Sentence, ICoqScriptSentence]) : Boolean = {
    val (text, synthetic) = s match {
      case Left((text, syn)) => (text, syn)
      case Right(s) => (s.getText, s.isSynthetic)
    }
    if (!synthetic) {
      ct.add(getHead, text, true) match {
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
          true
        case Interface.Fail((sid, loc, msg)) =>
          /* Adding this state to the document failed, so it doesn't have a
           * state ID -- and we need one to associate the error with. Make up
           * for this by inventing a fake state ID for this command */
          val ps = nextPrivateState
          stateIds :+= (makeSentenceID(s) -> ps)
          status += (ps -> Interface.Fail(sid, loc, msg))

          s.right.foreach(s => {
            /* Try to create an error marker */
            val (pos, len) = loc match {
              case Some((start, stop)) =>
                (start, stop - start)
              case None =>
                (0, s.getLength)
            }
            val issue = CoqEnforcement.Issue(
                ERR_ADD_FAILED, pos, len, msg, CoqEnforcement.Severity.Error)
            s.addIssue((issue, CoqEnforcement.Severity.Error))
          })

          /* Stop processing commands here */
          false
        case _ =>
          true
      }
    } else true
  }

  def attach(f : ICoqVernacFile, ib : String = "") = {
    file.foreach(_ => detach())
    initialisationBlock = Option(ib).filter(!_.isEmpty)
    file = Some(f)
    f.getModel.addListener(ChangeListener)
    sendInitialisationBlock()
    submitAll(f.getSentences)
  }
  def detach() = file.foreach(f => {
    f.getModel.removeListener(ChangeListener)
    file = None
    stateIds = Seq()
    goals = Map()
    status = Map()
    feedback = Map()
  })

  def query(s : ICoqScriptSentence,
      query : String) : Either[String, Seq[String]] = {
    val stid = makeSentenceID(s)
    stateIds.find(_._1 == stid).map(_._2) match {
      case Some(sid) =>
        val route = 32 + (scala.math.random * 32768.0).asInstanceOf[Int]
        var messages : Seq[String] = Seq()
        val collector = CoqIdeTopFeedbackListener {
          case Feedback(_, r, _, Feedback.Message((level, _, msg)))
              if r == route =>
            messages :+= msg
        }
        ct.addListener(collector)
        try {
          ct.query(route, query, sid) match {
            case Interface.Good(str) =>
              Right(messages)
            case Interface.Fail((_, _, msg)) =>
              Left(msg)
          }
        } finally {
          ct.removeListener(collector)
        }
      case None =>
        Left("Command not yet assigned a state by Coq")
    }
  }

  private var privateState = -1
  private def nextPrivateState() : Interface.state_id =
    try privateState finally privateState -= 1
}
object StateTracker {
  type SentenceID = (Int, Int)
  private def makeSentenceID(s : ICoqScriptSentence) : SentenceID =
    (s.getOffset, s.getText.hashCode)
  private def makeSentenceID(s : CoqSentence.Sentence) : SentenceID =
    (s._1.start, s._1.hashCode)
  private def makeSentenceID(
      s : Either[CoqSentence.Sentence, ICoqScriptSentence]) : SentenceID =
    s match {
      case Left(s) => makeSentenceID(s)
      case Right(s) => makeSentenceID(s)
    }

  final val ERR_ADD_FAILED =
    "dk.itu.coqoon.core.coqtop.coqidetop.StateTracker:addFailed"
}