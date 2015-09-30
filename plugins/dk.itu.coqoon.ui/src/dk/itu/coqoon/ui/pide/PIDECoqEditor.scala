package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.ui.{
  BaseCoqEditor, CoqGoalsContainer, CoqoonUIPreferences, ManifestIdentifiers}
import dk.itu.coqoon.ui.utilities.SupersedableTask

trait AdvancedNavigationHost {
  def getCommand(offset : Int) : Option[(Int, isabelle.Command)]
  def getEntities(command : isabelle.Command) :
      Seq[(isabelle.Text.Range, Responses.Entity)]
  def selectEntity(e : (isabelle.Text.Range, Responses.Entity)) : Unit
}

class PIDECoqEditor
    extends BaseCoqEditor with CoqGoalsContainer with OverlayRunner
                          with AdvancedNavigationHost {
  override def getCommand(offset : Int) : Option[(Int, isabelle.Command)] =
    CommandsLock synchronized {
      for (r @ (o, command) <- commands
          if offset >= o && offset <= o + command.length)
        return Some(r)
      None
    }
  override def getEntities(command : isabelle.Command) =
    CommandsLock synchronized {
      for ((range, elem) <- Responses.extractMarkup(lastSnapshot.get, command);
           entity <- Responses.extractEntity(elem))
        yield (range, entity)
    }
  override def selectEntity(e : (isabelle.Text.Range, Responses.Entity)) =
    CommandsLock synchronized {
      e match {
        case (r, Left((path_, (start, end)))) =>
          import dk.itu.coqoon.ui.utilities.UIUtils
          import dk.itu.coqoon.core.utilities.TryAdapt
          import org.eclipse.ui.ide.IDE
          import org.eclipse.core.runtime.Path
          import org.eclipse.core.resources.ResourcesPlugin
          import org.eclipse.core.filesystem.EFS
          import org.eclipse.jface.text.source.ISourceViewer
          val path = new Path(path_)
          val file = EFS.getLocalFileSystem.getStore(path)
          import org.eclipse.jface.text.source.ISourceViewer
          val page =
            UIUtils.getWorkbench.getActiveWorkbenchWindow.getActivePage
          Option(IDE.openInternalEditorOnFileStore(page, file)).
              flatMap(TryAdapt[ISourceViewer]) match {
            case Some(viewer) =>
              val s = start - 1
              val l = end - s
              viewer.revealRange(s, l)
              viewer.setSelectedRange(s, l)
            case _ =>
          }
        case (r, Right((exec_id, (start, end)))) =>
          val command = lastSnapshot.get.state.find_command(
              lastSnapshot.get.version, exec_id)
          command.foreach {
            case (_, command) =>
              commands.find(e => e._2 == command).foreach {
                case (offset, command) =>
                  val s = (offset + start) - 1
                  val l = end - start
                  getSourceViewer.revealRange(s, l)
                  getSourceViewer.setSelectedRange(s, l)
              }
          }
      }
    }

  val uiMoveTask = new SupersedableTask(200)

  private val reconciler = new PIDEReconciler(this)

  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.text.source.IVerticalRuler
  override protected def createSourceViewer(
      parent : Composite, ruler : IVerticalRuler, styles : Int) = {
    val viewer = super.createSourceViewer(parent, ruler, styles)
    reconciler.install(viewer)
    viewer.getTextWidget.addCaretListener(DocumentCaretListener)
    viewer
  }

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jface.text.source.AnnotationModel
  protected def getAnnotationModel() = Option(getDocumentProvider).flatMap(
      p => Option(p.getAnnotationModel(getEditorInput)).flatMap(
          TryCast[AnnotationModel]))

  private[pide] val session = SessionPool.makePooledSession

  override protected def dispose() = {
    session.stop
    super.dispose
  }

  def getViewer = super.getSourceViewer

  import isabelle.{XML, Text, Command, Session, Protocol, Document}

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    override def caretMoved(ev : CaretEvent) =
      uiMoveTask schedule {
        caretPing
      }
  }

  private var lastCommand : Option[isabelle.Command] = None

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  private def caretPing() =
    asyncExec {
      val caret = Option(getViewer).map(_.getTextWidget).filter(
          text => !text.isDisposed).map(_.getCaretOffset)
      val commandResultsAndMarkup = caret.flatMap(caret =>
        CommandsLock synchronized {
          val c = {
            var command : Option[(Int, Command)] = None
            commands.find {
              case q @ (o, c)
                  if o <= caret && !c.is_ignored =>
                command = Some(q)
                false
              case (o, c) if o <= caret =>
                /* Ignore ignored commands */
                false
              case _ =>
                true
            }
            command
          }
          lastSnapshot.flatMap(snapshot => c.map(
              c => (c,
                  Responses.extractResults(snapshot, c._2),
                  Responses.extractMarkup(snapshot, c._2))))
        })
      commandResultsAndMarkup match {
        case Some(((offset, command), results, markup)) =>
          val sameCommand = lastCommand.contains(command)

          markup.find(_._2.name == "goals") match {
            case Some((_, el))
                if !sameCommand || goals == None =>
              setGoals(Responses.extractGoals(el))
            case Some(el) => /* do nothing? */
            case None => setGoals(None)
          }

          if (!sameCommand) {
            import dk.itu.coqoon.ui.utilities.EclipseConsole
            import isabelle.XML.{Elem, Text}
            import isabelle.Markup
            for ((_, tree) <- results) tree match {
              case XML.Elem(
                  Markup(Markup.WRITELN_MESSAGE, properties),
                  List(t : Text))
                      if properties.toMap.get("source") != Some("goal") =>
                EclipseConsole.out.println(t.content)
              case f @ XML.Elem(Markup(Markup.ERROR_MESSAGE, _), _) =>
                Responses.extractError(f).map(_._2).foreach(
                    EclipseConsole.err.println)
              case _ =>
            }
            lastCommand = Option(command)
          }
        case _ =>
          setGoals(None)
          lastCommand = None
      }
    }

  import org.eclipse.jface.text.source.Annotation
  private var annotations : Map[Command, Annotation] = Map()

  private def commandsUpdated(changed : Seq[Command]) = {
    val changedResultsAndMarkup =
      (CommandsLock synchronized {
        val ls = lastSnapshot.get
        for (c <- changed)
          yield {
            ls.node.command_start(c) match {
              case Some(offset) if offset < ibLength =>
                /* If the command's in the initialisation block, then hide
                 * it, as annotating things the user can't see is unhelpful
                 * (and, incidentally, will make JFace throw exceptions). */
                None
              case h =>
                /* Otherwise, fix up the offset, if there was one, and keep
                 * this command and its metadata for further processing. */
                Some((h.map(_ - ibLength), c,
                    Responses.extractResults(ls, c),
                    Responses.extractMarkup(ls, c)))
            }
          }
      }).flatten

    asyncExec {
      val am =
        if (CoqoonUIPreferences.ProcessingAnnotations.get) {
          getAnnotationModel
        } else None
      am.foreach(
          model => Option(getViewer).map(_.getDocument).foreach(model.connect))

      import org.eclipse.jface.text.Position
      var toDelete : Seq[(Command, Option[Annotation])] = Seq()
      var annotationsToAdd : Seq[(Command, Annotation, Position)] = Seq()
      var errorsToAdd : Map[Int, ((Command, (Int, Int), String))] = Map()
      var errorsToDelete : Seq[Command] = Seq()

      try {
        for (i <- changedResultsAndMarkup) i match {
          case (Some(offset), command, results, markup) =>
            val complete =
              !(Protocol.Status.make(
                  markup.map(_._2.markup).iterator).is_running)

            /* Extract and display error messages */
            var commandHasErrors = false
            for ((_, tree) <- results) {
              import XML._
              tree match {
                case f : Elem if f.name == "error_message" =>
                  (getFile, Responses.extractError(f)) match {
                    case (Some(f), Some((id, msg, Some(start), Some(end)))) =>
                      errorsToAdd += id -> (command,
                          (offset + start - 1, offset + end - 1), msg)
                      commandHasErrors = true
                    case (Some(f), Some((id, msg, _, _))) =>
                      errorsToAdd += id -> (command,
                          (offset, offset + command.source.length), msg)
                      commandHasErrors = true
                    case _ =>
                  }
                case _ =>
              }
            }
            if (!commandHasErrors)
              errorsToDelete :+= command

            am.foreach(model => {
              import org.eclipse.jface.text.Position
              import org.eclipse.jface.text.source.Annotation
              import org.eclipse.jface.text.source.IAnnotationModelExtension
              (complete, annotations.get(command)) match {
                case (false, None) =>
                  /* This command hasn't finished but has no processing
                   * annotation; create a new one */
                  val an = new Annotation(
                      ManifestIdentifiers.ANNOTATION_PROCESSING,
                      false, "Processing proof")
                  annotationsToAdd :+= (command,
                      an, new Position(offset, command.source.length))
                case (false, Some(an)) =>
                  /* This command hasn't finished and has an existing
                   * processing annotation; delete it and replace it */
                  toDelete :+= (command, Some(an))
                  annotationsToAdd :+= (command,
                      an, new Position(offset, command.source.length))
                case (true, Some(an)) =>
                  /* This command has finished and has an existing processing
                   * annotation; delete it */
                  toDelete :+= (command, Some(an))
                case _ =>
              }
            })
          case (None, command, _, _) =>
            /* This command has been removed from the document; delete its
             * annotation, along with any errors it might have */
            toDelete :+= (command, annotations.get(command))
        }
      } finally {
        am.foreach(model => {
          import scala.collection.JavaConversions._
          val del =
            for ((command, Some(annotation)) <- toDelete)
              yield {
                annotations -= command
                annotation
              }
          val add =
            (for ((command, annotation, position) <- annotationsToAdd)
              yield {
                annotations += (command -> annotation)
                (annotation -> position)
              }).toMap
          model.replaceAnnotations(del.toArray, add)

          model.disconnect(getViewer.getDocument)
          getSourceViewer.invalidateTextPresentation
        })

        if (!toDelete.isEmpty || !errorsToAdd.isEmpty ||
            !errorsToDelete.isEmpty)
          getFile.foreach(file =>
            new UpdateErrorsJob(file,
                toDelete.map(_._1) ++ errorsToDelete,
                errorsToAdd.values.toSeq).schedule)
      }

      caretPing
    }
  }

  private object CommandsLock
  private var lastSnapshot : Option[Document.Snapshot] = None
  private[pide] var commands : Seq[(Int, isabelle.Command)] = Seq()

  session.addInitialiser(session =>
    session.commands_changed += Session.Consumer[Any]("Coqoon") {
      case changed : Session.Commands_Changed =>
        CommandsLock synchronized {
          lastSnapshot = getNodeName.map(n => session.snapshot(n))
          lastSnapshot.foreach(snapshot =>
            commands =
              (for (command <- snapshot.node.commands;
                    offset <- snapshot.node.command_start(command)
                      if offset >= ibLength)
                yield (offset - ibLength, command)).toSeq)
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

  import dk.itu.coqoon.core.coqtop.CoqSentence
  import dk.itu.coqoon.core.utilities.TotalReader
  import org.eclipse.ui.{IEditorInput, IFileEditorInput}
  override def doSetInput(input : IEditorInput) = {
    super.doSetInput(input)
    checkedUpdate(List())
  }

  import dk.itu.coqoon.ui.utilities.UIUtils.exec
  /* XXX: doing this synchronously on the UI thread makes me a bit nervous, but
   * we /do/ need to be able to access the text widget... */
  private def generateInitialEdits() = exec {
    val fi = TryCast[IFileEditorInput](getEditorInput)
    val text =
      Option(getSourceViewer).map(_.getTextWidget).map(_.getText) match {
        case Some(text) =>
          Some(text)
        case _ =>
          fi.map(fi => TotalReader.read(fi.getFile.getContents))
      }
    fi match {
      case Some(f) =>
        val file = f.getFile
        val nodeName = getNodeName.get

        import dk.itu.coqoon.core.model.ICoqModel
        val cp = ICoqModel.toCoqProject(file.getProject)
        val initialisationBlock =
          cp.getLoadPath.map(_.asCommand).mkString("", "\n", "\n")
        ibLength = initialisationBlock.length

        List[Document.Node.Edit[Text.Edit, Text.Perspective]](
            Document.Node.Clear(),
            Document.Node.Edits(List(
                Text.Edit.insert(0,
                    initialisationBlock + text.getOrElse("")))),
            getViewerPerspective)
      case _ =>
        List()
    }
  }

  import org.eclipse.core.resources.IFile
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)
  protected[ui] def getNodeName() =
    getFile.map(file => Document.Node.Name(file.getName))

  protected[ui] var ibLength : Int = 0

  private[pide] def checkedUpdate(
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
            edits.map(e => nodeName -> e) :+ (nodeName -> getViewerPerspective)
          slot.get.update(Document.Blobs.empty, textEdits, "coq")
        })
      }
      slot.get.phase
    })

  import dk.itu.coqoon.ui.pide.Overlay
  private var overlay : Option[(Overlay, OverlayListener)] = None
  override def setOverlay(overlay : Option[(Overlay, OverlayListener)]) =
    if (this.overlay != overlay) {
      this.overlay = overlay
      checkedUpdate(List())
    }

  import isabelle.{Text, Document}
  def getViewerPerspective() = {
    import org.eclipse.jface.text.Region
    val r = exec {
      Option(getViewer).flatMap {
        case v =>
          val (start, end) =
            (v.getTopIndexStartOffset, v.getBottomIndexEndOffset)
          if (end < start) {
            None
          } else Some(new Region(ibLength + start, end - start))
      }
    }
    val overlay = this.overlay match {
      case Some((o, _)) => o.wrap
      case _ => Document.Node.Overlays.empty
    }
    r match {
      case Some(r) =>
        Perspective.makePartial(r, overlay)
      case _ =>
        Perspective.makeEmpty(overlay)
    }
  }
}

object Perspective {
  import isabelle.{Text, Document}
  def makeEmpty(
      overlays : Document.Node.Overlays = Document.Node.Overlays.empty) =
    Document.Node.Perspective[Text.Edit, Text.Perspective](true,
        Text.Perspective.empty, overlays)
  import org.eclipse.jface.text.IRegion
  def makePartial(region : IRegion,
      overlays : Document.Node.Overlays = Document.Node.Overlays.empty) =
    Document.Node.Perspective[Text.Edit, Text.Perspective](true,
        Text.Perspective(
            Seq(Text.Range(
                region.getOffset,
                region.getOffset + region.getLength))), overlays)
  def makeFull(
      overlays : Document.Node.Overlays = Document.Node.Overlays.empty) =
    Document.Node.Perspective[Text.Edit, Text.Perspective](true,
        Text.Perspective.full, overlays)
}

import dk.itu.coqoon.core.utilities.{UniqueRule, JobUtilities}
import org.eclipse.core.resources.{IFile, WorkspaceJob}
import isabelle.Command
class UpdateErrorsJob(file : IFile,
    removed : Seq[Command], added : Seq[(Command, (Int, Int), String)])
    extends WorkspaceJob("Update PIDE markers") {
  setRule(JobUtilities.MultiRule(
      file.getWorkspace.getRuleFactory.markerRule(file),
      UpdateErrorsJob.rule))
  setSystem(true)

  import org.eclipse.core.runtime.{Status, IProgressMonitor}
  import org.eclipse.core.resources.{IMarker, IResource}
  override def runInWorkspace(monitor : IProgressMonitor) = {
    import dk.itu.coqoon.core
    import dk.itu.coqoon.core.debug.CoqoonDebugPreferences

    val m = file.findMarkers(core.ManifestIdentifiers.MARKER_PROBLEM,
        false, IResource.DEPTH_ZERO)
    val ids =
      removed.map(_.id.asInstanceOf[Int]) ++
          added.map(_._1.id.asInstanceOf[Int])
    /* Delete all errors associated with these commands */
    val deletedMarkers =
      for (i <- m if ids.contains(
          i.getAttribute("__command", Int.MaxValue)))
        yield {
          val id = i.getAttribute("__command", Int.MaxValue)
          i.delete
          (id, i)
        }
    import scala.collection.JavaConversions._
    val addedMarkers =
      for ((c, (start, end), msg) <- added)
        yield {
          val q = file.createMarker(
              core.ManifestIdentifiers.MARKER_PROBLEM)
          val id = c.id.asInstanceOf[Int]
          q.setAttributes(Map(
              IMarker.MESSAGE -> msg.replaceAll("\\s+", " ").trim,
              IMarker.SEVERITY -> IMarker.SEVERITY_ERROR,
              IMarker.LOCATION -> s"offset ${start}",
              IMarker.CHAR_START -> start,
              IMarker.CHAR_END -> end,
              IMarker.TRANSIENT -> true,
              "__command" -> id))
          (id, q)
        }
    CoqoonDebugPreferences.PIDEMarkers.log(
      s"UpdateErrorsJob(${removed}, ${added}):\n" +
      s"ids = ${ids},\n" +
      s"deleted ${deletedMarkers.toList}, added ${addedMarkers.toList}")
    Status.OK_STATUS
  }
}
private object UpdateErrorsJob {
  val rule = new dk.itu.coqoon.core.utilities.UniqueRule
}

import dk.itu.coqoon.ui.EventReconciler

private class PIDEReconciler(editor : PIDECoqEditor) extends EventReconciler {
  import EventReconciler._
  import org.eclipse.core.resources.{IMarker,IResource}

  override def reconcile(events : List[DecoratedEvent]) = {
    import isabelle._

    var edits : List[Text.Edit] = List()
    for (DecoratedEvent(ev, pre) <- events) {
      if (ev.fLength > 0)
        edits :+= Text.Edit.remove(editor.ibLength + ev.fOffset, pre)
      if (!ev.fText.isEmpty)
        edits :+= Text.Edit.insert(editor.ibLength + ev.fOffset, ev.fText)
    }
    editor.checkedUpdate(List(Document.Node.Edits(edits)))
  }
}
