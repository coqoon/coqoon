package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.ui.{
  BaseCoqEditor, CoqGoalsContainer, CoqoonUIPreferences, ManifestIdentifiers}
import dk.itu.coqoon.ui.utilities.SupersedableTask

trait PIDENavigationHost extends PIDESessionHost {
  def getEntities(command : isabelle.Command) :
      Seq[(isabelle.Text.Range, Responses.Entity)]
  def selectEntity(e : Responses.Entity) : Unit
}
object PIDENavigationHost {
  def openLeft(left : (String, (Int, Int))) = {
    val (path_, (start, end)) = left
    import dk.itu.coqoon.ui.utilities.UIUtils
    import dk.itu.coqoon.core.utilities.TryAdapt
    import org.eclipse.ui.ide.IDE
    import org.eclipse.core.runtime.Path
    import org.eclipse.core.resources.ResourcesPlugin
    import org.eclipse.core.filesystem.EFS
    import org.eclipse.jface.text.source.ISourceViewer
    val path = new Path(path_)
    val file = EFS.getLocalFileSystem.getStore(path)
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
  }
}

class PIDECoqEditor
    extends BaseCoqEditor with CoqGoalsContainer with PIDESessionHost
                          with PIDENavigationHost {
  private[pide] def getLastSnapshot() =
    executeWithCommandsLock {
      lastSnapshot
    }

  override def getEntities(command : isabelle.Command) = {
    val ls = getLastSnapshot.get
      for ((range, elem) <- Responses.extractMarkup(ls, command);
           entity <- Responses.extractEntity(elem))
        yield (range, entity)
  }
  override def selectEntity(e : Responses.Entity) = {
    val ls = getLastSnapshot.get
    e match {
      case Left(l) => PIDENavigationHost.openLeft(l)
      case Right((exec_id, (start, end))) =>
        val command = ls.state.find_command(ls.version, exec_id)
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

  import org.eclipse.jface.text.IViewportListener
  object ViewportListener extends IViewportListener {
    override def viewportChanged(x : Int) =
      if (CoqoonUIPreferences.UsePerspective.get) {
        uiMoveTask schedule {
          checkedUpdate(List())
        }
      }
  }

  private val reconciler = new PIDEReconciler(this)

  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.text.source.IVerticalRuler
  override protected def createSourceViewer(
      parent : Composite, ruler : IVerticalRuler, styles : Int) = {
    val viewer = super.createSourceViewer(parent, ruler, styles)
    reconciler.install(viewer)
    viewer.getTextWidget.addCaretListener(DocumentCaretListener)
    viewer.addViewportListener(ViewportListener)
    viewer
  }

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jface.text.source.AnnotationModel
  protected def getAnnotationModel() = Option(getDocumentProvider).flatMap(
      p => Option(p.getAnnotationModel(getEditorInput)).flatMap(
          TryCast[AnnotationModel]))

  override protected def dispose() = {
    session.stop
    super.dispose
  }

  def getViewer = super.getSourceViewer

  import isabelle.{Text, Command, Protocol, Document}

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    override def caretMoved(ev : CaretEvent) =
      uiMoveTask schedule {
        caretPing
      }
  }

  private var lastCommand : Option[Command] = None

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  private def caretPing() =
    asyncExec {
      val ls = getLastSnapshot
      val caret = Option(getViewer).map(_.getTextWidget).filter(
          text => !text.isDisposed).map(_.getCaretOffset)
      val commandResultsAndMarkup = caret.flatMap(caret => {
        val c = findCommand(caret)
        ls.flatMap(snapshot => c.map(
            c => (c,
                Responses.extractResults(snapshot, c._2),
                Responses.extractMarkup(snapshot, c._2))))
      })
      commandResultsAndMarkup match {
        case Some(((offset, command), results, markup)) =>
          val sameCommand = lastCommand.contains(command)

          /* Preserve the contents of the goal viewer if the currently selected
           * command has an error */
          val hasErrors =
            results.exists(r => Responses.extractError(r._2) != None)
          markup.find(_._2.name == "goals") match {
            case Some((_, el))
                if !sameCommand || goals == None =>
              setGoals(Responses.extractGoals(el))
            case None if !hasErrors => setGoals(None)
            case _ => /* do nothing */
          }

          if (!sameCommand) {
            import dk.itu.coqoon.ui.utilities.EclipseConsole
            for ((_, tree) <- results) {
              Responses.extractWritelnMessage(tree).foreach(message =>
                  EclipseConsole.out.println(message))
              Responses.extractError(tree).foreach(error =>
                  EclipseConsole.err.println(error._2))
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

  override protected def commandsUpdated(changed : Seq[Command]) = {
    val ls = getLastSnapshot.get
    val changedResultsAndMarkup = ({
      commands =
        (for (command <- ls.node.commands;
              offset <- ls.node.command_start(command)
                if offset >= ibLength)
          yield (offset - ibLength, command)).toSeq
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
              (getFile, Responses.extractError(tree)) match {
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
            }
            if (!commandHasErrors)
              errorsToDelete :+= command

            val oldAnnotation = annotations.get(command)
            val newAnnotation =
              if (!complete) {
                  Some(new Annotation(
                      ManifestIdentifiers.Annotations.PROCESSING,
                      false, "Processing proof"))
              } else None

            /* If the old and new annotations have the same type (or, for that
             * matter, if neither of them exists), then do nothing */
            if (newAnnotation.map(_.getType) != oldAnnotation.map(_.getType)) {
              oldAnnotation.foreach(an => toDelete :+= (command, Some(an)))
              newAnnotation.foreach(an => annotationsToAdd :+= (command,
                  an, new Position(offset, command.source.length)))
            }
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
                ls.node.commands,
                toDelete.map(_._1) ++ errorsToDelete,
                errorsToAdd.values.toSeq).schedule)
      }

      lastCommand match {
        case None =>
          caretPing
        case Some(c) if changed.contains(c) =>
          caretPing
        case _ =>
      }
    }
  }

  /* Synchronized on PIDESessionHost's CommandsLock */
  private[pide] var commands : Seq[(Int, isabelle.Command)] = Seq()

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
  protected def generateInitialEdits() = exec {
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
          cp.getLoadPath.flatMap(_.asCommands).mkString("", "\n", "\n")
        ibLength = initialisationBlock.length

        List[Document.Node.Edit[Text.Edit, Text.Perspective]](
            Document.Node.Clear(),
            Document.Node.Edits(List(
                Text.Edit.insert(0,
                    initialisationBlock + text.getOrElse("")))),
            getPerspective)
      case _ =>
        List()
    }
  }

  import org.eclipse.core.resources.IFile
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)
  override protected def getNodeName() =
    getFile.map(file => Document.Node.Name(file.getName))

  protected[ui] var ibLength : Int = 0

  override protected def getPerspective() = {
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
    val overlay = getOverlay match {
      case Some((o, _)) => o.wrap
      case _ => Document.Node.Overlays.empty
    }
    r match {
      case Some(r) =>
        Perspective.makePartial(r, overlay)
      case _ =>
        Perspective.makeFull(overlay)
    }
  }

  private def getCommandAtDocumentOffset(offset : Int) =
    getLastSnapshot.map(_.node).map(_.command_iterator(offset)) match {
      case Some(it) if it.hasNext =>
        val (c, o) = it.next
        Some((o, c))
      case _ =>
        None
    }

  private def documentCommandToPresentationCommand(c : (Int, Command)) =
    Option(c).collect {
      case (o, c) if o >= ibLength => (o - ibLength, c)
    }

  private def getCommandAtPresentationOffset(offset : Int) =
    getCommandAtDocumentOffset(offset + ibLength).flatMap(
        documentCommandToPresentationCommand)

  override def findCommand(offset : Int) = {
    var itc = getCommandAtPresentationOffset(offset)
    while (itc.exists(_._2.is_ignored))
      itc = getCommandAtPresentationOffset(itc.get._1 - 1)
    itc
  }

  private[pide] def getFirstCommand() = {
    var itc = getCommandAtDocumentOffset(ibLength)
    while (itc.exists(_._2.is_ignored)) {
      val (o, c) = itc.get
      itc = getCommandAtDocumentOffset(o + c.length)
    }
    itc.flatMap(documentCommandToPresentationCommand)
  }

  override protected[pide] def executeWithCommandsLock[A](
      f : => A) = super.executeWithCommandsLock(f)
  private[pide] def getSession() = session
}

object Perspective {
  import isabelle.{Text, Document}
  import org.eclipse.jface.text.IRegion
  def makePartial(region : IRegion,
      overlays : Document.Node.Overlays = Document.Node.Overlays.empty) =
    if (CoqoonUIPreferences.UsePerspective.get) {
      Document.Node.Perspective[Text.Edit, Text.Perspective](true,
          Text.Perspective(
              Seq(Text.Range(
                  region.getOffset,
                  region.getOffset + region.getLength))), overlays)
    } else makeFull(overlays)
  def makeFull(
      overlays : Document.Node.Overlays = Document.Node.Overlays.empty) =
    Document.Node.Perspective[Text.Edit, Text.Perspective](true,
        Text.Perspective.full, overlays)
}

import dk.itu.coqoon.core.utilities.{UniqueRule, JobUtilities}
import org.eclipse.core.resources.{IFile, WorkspaceJob}
import isabelle.Command
class UpdateErrorsJob(file : IFile, commands : Set[Command],
    removed : Seq[Command], added : Seq[(Command, (Int, Int), String)])
    extends WorkspaceJob("Update PIDE markers") {
  import UpdateErrorsJob._

  setRule(JobUtilities.MultiRule(
      file.getWorkspace.getRuleFactory.markerRule(file),
      rule))
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
      (for (i <- m)
        yield {
          val commandID = i.getAttribute("__command", Int.MaxValue)
          val delete =
            if (ids.contains(commandID)) {
              /* The command associated with this marker has been deleted, or a
               * new marker is going to be created for it. In either case, this
               * marker needs to be deleted */
              true
            } else if (!commands.exists(_.id == commandID)) {
              /* The command associated with this marker doesn't actually exist
               * in the document; delete it. (As the builder doesn't care about
               * PIDE command identifiers, this also has the effect of deleting
               * any markers that it may have created.) */
              true
            } else false
          if (delete) {
            i.delete
            Some((commandID, i))
          } else None
        }).flatten
    import scala.collection.JavaConversions._
    val addedMarkers =
      for ((c, (start, end), msg) <- added)
        yield {
          val q = file.createMarker(
              core.ManifestIdentifiers.MARKER_PROBLEM)
          val id = c.id.asInstanceOf[Int]
          q.setAttributes(Map(
              IMarker.MESSAGE -> reduceError(msg).replaceAll("\\s+", " ").trim,
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
  /* Sync with CoqBuilder */
  def reduceError(s : String) = s.lines.toSeq.takeRight(20).mkString("\n")

  val rule = new dk.itu.coqoon.core.utilities.UniqueRule
}

import dk.itu.coqoon.ui.EventReconciler

private class PIDEReconciler(editor : PIDECoqEditor) extends EventReconciler {
  import EventReconciler._
  import org.eclipse.core.resources.{IMarker,IResource}

  override def reconcile(events : List[DecoratedEvent]) = {
    import isabelle._

    var earliestOffset = Int.MaxValue
    var edits : List[Text.Edit] = List()
    for (DecoratedEvent(ev, pre) <- events) {
      if (ev.fLength > 0)
        edits :+= Text.Edit.remove(editor.ibLength + ev.fOffset, pre)
      if (!ev.fText.isEmpty)
        edits :+= Text.Edit.insert(editor.ibLength + ev.fOffset, ev.fText)
      earliestOffset = Math.min(earliestOffset, ev.fOffset)
    }

    val stop = editor.getFirstCommand.exists(
        c => earliestOffset <= c._1 + c._2.length)
    if (stop)
      editor.getSession.stop

    editor.checkedUpdate(List(Document.Node.Edits(edits)))
  }
}
