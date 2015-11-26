package dk.itu.coqoon.ui.pide

trait PIDESessionHost extends OverlayRunner {
  protected val session = SessionPool.makePooledSession

  import org.eclipse.jface.text.source.AnnotationModel
  /* XXX: does the annotation model really belong in this trait? */
  protected def getAnnotationModel() : Option[AnnotationModel]

  import dk.itu.coqoon.ui.pide.Overlay
  private var overlay : Option[(Overlay, OverlayListener)] = None
  protected def getOverlay() : Option[(Overlay, OverlayListener)] = overlay
  override def setOverlay(overlay : Option[(Overlay, OverlayListener)]) =
    if (this.overlay != overlay) {
      this.overlay = overlay
      checkedUpdate(List())
    }

  import isabelle.{Text, Command, Session, Document}

  protected def getNodeName() : Option[Document.Node.Name]
  protected def getPerspective() :
      Document.Node.Perspective[Text.Edit, Text.Perspective]
  protected def commandsUpdated(changed : Seq[Command]) : Unit
  protected def generateInitialEdits() :
      List[Document.Node.Edit[Text.Edit, Text.Perspective]]

  private object CommandsLock
  protected var lastSnapshot : Option[Document.Snapshot] = None
  protected def executeWithCommandsLock[A](f : => A) =
    CommandsLock synchronized f

  private object DeadLock /* (no pun intended) */ {
    var dead : Boolean = false
  }

  session.addInitialiser(session => {
    session.commands_changed += Session.Consumer[Any]("Coqoon") {
      case changed : Session.Commands_Changed =>
        CommandsLock synchronized {
          lastSnapshot = getNodeName.map(n => session.snapshot(n))
        }
        commandsUpdated(changed.commands.toSeq)

        overlay match {
          case Some((o @ Overlay(command, _, _), listener))
              if changed.commands.contains(command) =>
            Responses.extractQueryResult(
                lastSnapshot.get, command, o.id).foreach(listener.onResult)
          case _ =>
        }
      case _ =>
    }
    session.syslog_messages += Session.Consumer("Coqoon") {
      case o : isabelle.Prover.Output
          if o.is_exit && o.properties.contains("return_code" -> "1") =>
        DeadLock synchronized {
          DeadLock.dead = true
          val lastLines = session.syslog_content.lines.toSeq.map(
              _.trim).filterNot(_.isEmpty).reverse.take(50).reverse
          dk.itu.coqoon.ui.utilities.EclipseConsole.err.println(
              s"Coq session for ${getNodeName.getOrElse("(unknown)")} " +
              "stopped unexpectedly:\n" + lastLines.mkString("\n"))
          getAnnotationModel.foreach(_.removeAllAnnotations)
          /* Raise an error? */
        }
      case _ =>
    }
  })

  def checkedUpdate(
      edits_ : List[Document.Node.Edit[Text.Edit, Text.Perspective]]) =
    if (!(DeadLock synchronized (DeadLock.dead))) {
      session.executeWithSessionLockSlot(slot => {
        val submitInitialEdits =
          slot.asOption match {
            case None =>
              session.start
              true
            case Some(s)
                if s.phase == Session.Failed || s.phase == Session.Inactive =>
              /* The prover stopped or was killed off, so any existing progress
               * markers are invalid; clear them */
              getAnnotationModel.foreach(_.removeAllAnnotations)
              s.stop
              slot.clear
              session.start
              true
            case _ =>
              false
          }
        if (slot.get.phase == Session.Ready) {
          val edits =
            if (submitInitialEdits) {
              /* Since the generateInitialEdits method builds an initial edit
               * including the current body of the document, the text edits
               * passed to this method are redundant! As a result, we need to
               * filter those edits out to avoid getting out of sync. */
              val eds = edits_.filterNot {
                case e : Document.Node.Edits[
                  Text.Edit, Text.Perspective] => true
                case _ => false
              }
              generateInitialEdits ++ eds
            } else edits_
          getNodeName.foreach(nodeName => {
            val textEdits : List[Document.Edit_Text] =
              edits.map(e => nodeName -> e) :+ (nodeName -> getPerspective)
            slot.get.update(Document.Blobs.empty, textEdits, "coq")
          })
        }
        slot.get.phase
      })
    }

  /* Finds the PIDE command at @offset, if there is one, and returns it and its
   * offset in the complete PIDE document.
   *
   * @offset should be a presentation offset (for example, the position of a
   * caret in a StyledText widget). Depending on the editor, this may not
   * necessarily correspond directly to a PIDE document position!
   *
   * If the command at the specified offset is ignored (i.e., it represents a
   * comment or a block of whitespace), this method may instead return the
   * first preceding non-ignored command. That is, when this method returns
   * something other than None, the returned command is safe to use as the
   * target of a PIDE query. */
  def findCommand(offset : Int) : Option[(Int, Command)]
}