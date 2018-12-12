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
    var stateIds : Seq[(ICoqScriptSentence, Interface.state_id)] = Seq()
    def appendState(s : ICoqScriptSentence, sid : Interface.state_id) =
      stateIds :+= (s, sid)
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

  def sentenceKnown(s : ICoqScriptSentence) = (getStateID(s).exists(_ != 0))

  private def getStateID(
      s : ICoqScriptSentence) : Option[Interface.state_id] =
    Lock synchronized {
      import Lock._
      stateIds.find(_._1 == s).map(_._2)
    }

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
    Lock synchronized {
      val head = Lock.stateIds.lastIndexWhere(_._2 > 0)
      if (head != -1) {
        Lock.stateIds(head)._2
      } else 1
    }

  private object ChangeListener
      extends CoqElementChangeListener with CoqIdeTopFeedbackListener {
    override def coqElementChanged(ev : CoqElementEvent) =
      ev match {
        case CoqFileContentChangedEvent(
            f : ICoqVernacFile) if file.contains(f) =>
          val (ibCount, zippedSentences) = Lock synchronized {
            /* Count, and filter out, the initialisation block states */
            val ibCount = Lock.stateIds.indexWhere(_._1 != null)
            val stateIds = Lock.stateIds.drop(ibCount)
            (ibCount, f.getSentences.map(s => Some(s)).zipAll(
                stateIds.map(s => Some(s._1)), None, None))
          }
          val divergence = zippedSentences.indexWhere {
            case (a, b) if a != b =>
              true
            case (a, b) =>
              false
          }
          if (divergence != -1) {
            val rewound = Lock synchronized {
              import Lock._
              /* divergence is the index at which the user-visible part of the
               * document has changed, but let's not forget about the
               * initialisation block! */
              removeData(stateIds.drop(ibCount + divergence).map(_._2))
              stateIds = stateIds.take(ibCount + divergence)

              /* Having done that, the new head *might* be a state for which
               * EditAt will fail; check the status to find out. */
              var rewound = 0
              var continue = true
              while (continue && !stateIds.isEmpty) {
                val (_, sid) = stateIds.last
                status.get(sid) match {
                  case Some(Interface.Good(_)) =>
                    /* This is a valid head */
                    continue = false
                  case Some(Interface.Fail(_)) | None =>
                    /* This is either an invalid head or a synthetic one --
                     * remove it (we'll resubmit it in a moment anyway) */
                    stateIds = stateIds.dropRight(1)
                    rewound += 1
                }
              }
              rewound
            }
            ct.editAt(getHead) match {
              case Interface.Good(_) =>
              case Interface.Fail((sid, _, msg)) =>
                new InvalidManagedStateError(
                    s"Coqoon tried to backtrack into an error state ($sid) " +
                    "-- shouldn't happen!\n" + msg).printStackTrace()
            }
            if (getHead == 1)
              sendInitialisationBlock
            f.getSentences.drop(divergence - rewound).foreach(Submitter.add)
          }
        case _ =>
    }
    def updateModel(s : ICoqScriptSentence, f : Feedback) =
      println(s.getText.trim, f)
    def onStateAssigned(
        s : ICoqScriptSentence, sid : Interface.state_id) =
      Lock.synchronized {
        Lock.feedback.get(sid)
      }.getOrElse(MBuffer()).foreach(f => updateModel(s, f))
    override def onFeedback(f : Feedback) = {
      Lock synchronized {
        import Lock._

        /* Feedback might arrive before we've processed the return value of
         * add(), so instead of going through stateIds to work out which
         * command this message is associated with, just stash it away
         * immediately */
        if (!feedback.contains(f.state))
          feedback += (f.state -> MBuffer())
        feedback.get(f.state).get += f

        /* Update the model using this feedback, if we know what sentence it
         * belongs to. If we don't, then Submitter will let us know eventually
         * by calling onStateAssigned  */
        stateIds.find(_._2 == f.state) foreach {
          case (null, _) => /* do nothing */
          case (s, _) => updateModel(s, f)
        }
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
        (s, ct.add(getHead, text, true)) match {
          case (Right(s), Interface.Good((sid, (Left(()), msg)))) =>
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
              appendState(s, sid)
              status += (sid -> st)
              goalData.foreach(gs => goals += (sid -> gs))
            }
            ChangeListener.onStateAssigned(s, sid)
          case (Left(_), Interface.Good((sid, (Left(()), _)))) =>
            /* This was an initialisation block sentence, and so not backed by
             * a model object -- but we need to note sid in the stateIds "map",
             * because it's the new head. Store a dummy entry */
            Lock synchronized {
              import Lock._
              appendState(null, sid)
            }
          case (Right(s), Interface.Fail((0, loc, msg))) =>
            /* Something went wrong when we tried to add this command; stash
             * the error away with a private state ID of our own creation and
             * trigger a model update to draw the error */
            val (pos, len) = loc match {
              case Some((start, stop)) =>
                (start, stop - start)
              case None =>
                (0, s.getLength)
            }
            val issue = CoqEnforcement.Issue(
                ERR_ADD_FAILED, pos, len, msg, CoqEnforcement.Severity.Error)
            s.addIssue((issue, CoqEnforcement.Severity.Error))
            val ps = Lock synchronized {
              import Lock._
              /* Adding this state to the document failed, so it doesn't have
               * a state ID -- and we need one to associate the failed status
               * with. Invent a fake one for this purpose */
              val ps = nextPrivateState
              appendState(s, ps)
              status += (ps -> Interface.Fail(0, loc, msg))
              ps
            }
            ChangeListener.onStateAssigned(s, ps)

            if (msg.startsWith("Currently, the parsing api")) {
              new InvalidManagedStateError(
                  "Coqoon directed Coq to the wrong point in the " +
                  "document -- shouldn't happen!\n" + msg).printStackTrace()
            }
          case (Left(s), Interface.Fail((sid, loc, msg))) =>
            new InvalidManagedStateError(
                "Coqoon generated an initialisation block sentence " +
                s"(${s.toString}) with errors -- shouldn't " +
                "happen!\n" + msg).printStackTrace()
          case _ =>
        }
      } else {
        s.right.foreach(s => {
          /* Synthetic model sentences should go into our view of the document
           * to make sure it doesn't get too out of sync with the model, but
           * they don't get a real state ID -- or even a convincing fake state
           * ID */
          Lock synchronized Lock.appendState(s, 0)
        })
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
  def detach() = file.foreach(f => {
    f.getModel.removeListener(ChangeListener)
    file = None
    Lock synchronized {
      import Lock._
      stateIds = Seq()
      goals = Map()
      status = Map()
      feedback = Map()
    }
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
  final val ERR_ADD_FAILED =
    "dk.itu.coqoon.core.coqtop.coqidetop.StateTracker:addFailed"
}

class InvalidManagedStateError(val msg : String) extends Error(msg)