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

  session.addInitialiser(session =>
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
    })

  def checkedUpdate(
      edits_ : List[Document.Node.Edit[Text.Edit, Text.Perspective]]) =
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
            generateInitialEdits ++ edits_
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