package dk.itu.coqoon.core.coqtop.coqidetop

import scala.collection.DefaultMap
import scala.collection.mutable.{Buffer => MBuffer}

import dk.itu.coqoon.core.model.CoqEnforcement
import dk.itu.coqoon.core.model.ICoqVernacFile
import dk.itu.coqoon.core.model.{ICoqScriptGroup, ICoqScriptSentence}
import dk.itu.coqoon.core.model.CoqElementChangeListener
import dk.itu.coqoon.core.model.{CoqElementEvent, CoqFileContentChangedEvent}
import dk.itu.coqoon.core.coqtop.CoqSentence
import dk.itu.coqoon.core.utilities.BatchCollector

class StateTracker(
    ct : CoqIdeTop_v20170413) {
  import StateTracker._

  var file : Option[ICoqVernacFile] = None
  var initialisationBlock : Option[String] = None

  private object Lock {
    var stateIds : Seq[(SentenceID, Interface.state_id)] = Seq()
    var goals : Map[Interface.state_id, Interface.goals] = Map()
    var status :
        Map[Interface.state_id, Interface.value[Interface.status]] = Map()
    var feedback : Map[Interface.state_id, MBuffer[Feedback]] = Map()
    var privateState = -1
  }

  private def nextPrivateState() : Interface.state_id =
    Lock synchronized (try Lock.privateState finally Lock.privateState -= 1)

  private def sendInitialisationBlock() =
    initialisationBlock.foreach(ib => {
      CoqSentence.getNextSentences(ib, 0, ib.length).foreach(Submitter.add)
    })

  def sentenceKnown(s : ICoqScriptSentence) = (getStateID(s) != None)

  private def _getStateID(
      sentenceID : SentenceID) : Option[Interface.state_id] =
    Lock synchronized {
      import Lock._
      stateIds.find(_._1 == sentenceID).map(_._2)
    }
  private def getStateID(
      s : ICoqScriptSentence) : Option[Interface.state_id] =
    _getStateID(makeSentenceID(s))
  private def getStateID(
      s : CoqSentence.Sentence) : Option[Interface.state_id] =
    _getStateID(makeSentenceID(s))

  def getGoals(s : ICoqScriptSentence) =
    Lock synchronized getStateID(s).flatMap(Lock.goals.get)
  def getStatus(s : ICoqScriptSentence) =
    Lock synchronized getStateID(s).flatMap(Lock.status.get)
  def getFeedback(s : ICoqScriptSentence) =
    Lock synchronized getStateID(s).flatMap(Lock.feedback.get)

  private def removeData(sids : Seq[Interface.state_id]) =
    Lock synchronized sids.foreach(sid => {
      Lock.goals -= sid
      Lock.status -= sid
      Lock.feedback -= sid
    })

  private def getHead() : Interface.state_id =
    Lock synchronized Lock.stateIds.lastOption.map(_._2).getOrElse(1)

  private object ChangeListener
      extends CoqElementChangeListener with CoqIdeTopFeedbackListener {
    override def coqElementChanged(ev : CoqElementEvent) =
      ev match {
        case CoqFileContentChangedEvent(
            f : ICoqVernacFile) if file.contains(f) =>
          val zippedIDs = Lock synchronized {
            f.getSentences.map(
                s => Some(makeSentenceID(s))).zipAll(
                    Lock.stateIds.map(s => Some(s._1)), None, None)
          }
          val divergence = zippedIDs.indexWhere {
            case (a, b) if a != b =>
              true
            case (a, b) =>
              false
          }
          if (divergence != -1) {
            Lock synchronized {
              removeData(Lock.stateIds.drop(divergence).map(_._2))
              Lock.stateIds = Lock.stateIds.take(divergence)
            }
            ct.editAt(getHead)
            if (getHead == 1)
              sendInitialisationBlock
            f.getSentences.drop(divergence).foreach(Submitter.add)
          }
        case _ =>
    }
    override def onFeedback(f : Feedback) = {
      Lock synchronized {
        import Lock._

        /* Feedback might arrive before we've processed the return value of
         * add(), so instead of going through stateIds to work out which command
         * this message is associated with, just stash it away immediately */
        if (!feedback.contains(f.state))
          feedback += (f.state -> MBuffer())
        feedback.get(f.state).get += f
      }

      /* Resend this feedback to our own listeners, with the expectation that
       * they'll basically ignore its content and will use a SupersedableTask
       * or something to update the presentation based on the current tracked
       * state */
      listeners.foreach(_.onFeedback(f))
    }
  }
  ct.addListener(ChangeListener)

  private object Submitter extends BatchCollector[
      Either[CoqSentence.Sentence, ICoqScriptSentence]] {
    def add(s : CoqSentence.Sentence) : Unit = add(Left(s))
    def add(s : ICoqScriptSentence) : Unit = add(Right(s))

    def process(sentences :
        List[Either[CoqSentence.Sentence, ICoqScriptSentence]]) =
      sentences.foreach(submit)

    def submit(
        s : Either[CoqSentence.Sentence, ICoqScriptSentence]) = {
      val (text, synthetic) = s match {
        case Left((text, syn)) => (text, syn)
        case Right(s) => (s.getText, s.isSynthetic)
      }
      if (!synthetic) {
        ct.add(getHead, text, true) match {
          case Interface.Good((sid, (Left(()), msg))) =>
            val st = ct.status(false)
            val goalData = st match {
              case Interface.Good(Interface.status(_, Some(_), _, _)) =>
                ct.goal() match {
                  case Interface.Good(Some(gs)) =>
                    Some(gs)
                  case _ =>
                    None
                }
              case _ =>
                None
            }

            Lock synchronized {
              import Lock._
              stateIds :+= (makeSentenceID(s) -> sid)
              status += (sid -> st)
              goalData.foreach(gs => goals += (sid -> gs))
            }
            true
          case Interface.Fail((sid, loc, msg)) =>
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

            Lock synchronized {
              import Lock._
              /* Adding this state to the document failed, so it doesn't have a
               * state ID -- and we need one to associate the error with. Make
               * up for this by inventing a fake state ID for this command */
              val ps = nextPrivateState
              stateIds :+= (makeSentenceID(s) -> ps)
              status += (ps -> Interface.Fail(sid, loc, msg))
            }
          case _ =>
        }
      }
    }
  }

  def attach(f : ICoqVernacFile, ib : String = "") = {
    file.foreach(_ => detach())
    initialisationBlock = Option(ib).filter(!_.isEmpty)
    file = Some(f)
    f.getModel.addListener(ChangeListener)
    sendInitialisationBlock()
    f.getSentences.foreach(Submitter.add)
  }
  def detach() = file.foreach(f => Lock synchronized {
    import Lock._
    f.getModel.removeListener(ChangeListener)
    file = None
    stateIds = Seq()
    goals = Map()
    status = Map()
    feedback = Map()
  })

  def query(s : ICoqScriptSentence,
      query : String) : Either[String, Seq[String]] =
    getStateID(s) match {
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

  protected var listeners : Set[CoqIdeTopFeedbackListener] = Set()
  def addListener(l : CoqIdeTopFeedbackListener) = (listeners += l)
  def removeListener(l : CoqIdeTopFeedbackListener) = (listeners -= l)
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