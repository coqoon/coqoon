package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.ui.{
  BaseCoqEditor, CoqGoalsContainer, CoqoonUIPreferences, ManifestIdentifiers}
import dk.itu.coqoon.ui.text.Region
import dk.itu.coqoon.ui.utilities.SupersedableTask
import dk.itu.coqoon.core.model.{CoqEnforcement, CoqEnforcementContext}

class PIDECoqEditor
    extends BaseCoqEditor with CoqGoalsContainer with PIDESessionHost {
  import org.eclipse.jface.text.IViewportListener
  object ViewportListener extends IViewportListener {
    val task = new SupersedableTask(200)
    override def viewportChanged(unused : Int) =
      if (CoqoonUIPreferences.UsePerspective.get) {
        task schedule {
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

  import isabelle.{Text, Command, Protocol, Document}

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    val task = new SupersedableTask(200)
    override def caretMoved(ev : CaretEvent) =
      task schedule {
        caretPing
      }
  }

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  private var lastCommand : Option[Command] = None
  session.addInitialiser(_ => asyncExec {
    lastCommand = None
    annotations = Map()
    getAnnotationModel.foreach(_.removeAllAnnotations)
  })

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
      for (c <- changed if !c.is_ignored)
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
      val workingCopy = getWorkingCopy.get.get

      import org.eclipse.jface.text.Position
      var toDelete : Seq[(Command, Option[Annotation])] = Seq()
      var annotationsToAdd : Seq[(Command, Annotation, Position)] = Seq()
      var errorsToAdd : Map[Int, ((Command, (Int, Int), String))] = Map()
      var errorsToDelete : Seq[Command] = Seq()

      val am =
        if (CoqoonUIPreferences.ProcessingAnnotations.get) {
          getAnnotationModel
        } else None
      try {
        for (i <- changedResultsAndMarkup) i match {
          case (Some(offset), command, results, markup) =>
            val complete =
              !(Protocol.Status.make(
                  markup.map(_._2.markup).iterator).is_running)

            lazy val sentence = workingCopy.getSentenceAt(offset).get
            /* This sorts out the difference between the offset of the Coqoon
             * model sentence, which may include whitespace, and the PIDE span
             * offset, which doesn't */
            lazy val diff = offset - sentence.getOffset

            /* Extract and display error messages and warnings */
            import dk.itu.coqoon.core.model.CoqEnforcement.{Issue, Severity}
            /* We use a map here in order to merge errors with the same ID
             * together */
            var errors : Map[Int, Issue] = Map()
            for ((_, tree) <- results) {
              Responses.extractError(tree) match {
                case (Some((id, msg, Some(start), Some(end)))) =>
                  errors += id -> Issue("interactive/pide-error",
                      diff + start - 1, (end - start),
                      msg, Severity.Error)
                case Some((id, msg, _, _)) =>
                  errors += id -> Issue("interactive/pide-error",
                      diff, command.source.length,
                      msg, Severity.Error)
                case _ =>
                  Responses.extractWarning(tree) match {
                    case Some((id, msg, Some(start), Some(end))) =>
                      errors += id -> Issue("interactive/pide-warning",
                          diff + start - 1, (end - start),
                          msg, Severity.Warning)
                    case Some((id, msg, _, _)) =>
                      errors += id -> Issue("interactive/pide-warning",
                          diff, command.source.length,
                          msg, Severity.Warning)
                    case _ =>
                  }
              }
            }
            if (!errors.isEmpty)
              sentence.setIssues(errors.values.toSeq.map(
                  issue => (issue -> issue.severityHint)).toMap)

            sentence.setEntities(
              (for ((range, elem) <- markup;
                   location <- getFile.map(_.getLocation);
                   entity <- PIDEEntity.makeModelEntity(
                       ibLength, ls, location, elem))
                yield (diff + range.start, range.length) -> entity).toMap)

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

        if (!del.isEmpty || !add.isEmpty)
          am.foreach(_.replaceAnnotations(del.toArray, add))
      }

      lastCommand match {
        case None =>
          caretPing
        case Some(c) if changed.contains(c) =>
          caretPing
        case _ =>
      }

      import dk.itu.coqoon.core.model.CoqEnforcement
      for ((el, issues) <- CoqEnforcement.check(workingCopy,
          PIDEEnforcementContext))
        el.addIssues(issues)
    }
  }

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
    val overlay = getOverlay match {
      case Some((o, _)) => o.wrap
      case _ => Document.Node.Overlays.empty
    }
    if (CoqoonUIPreferences.UsePerspective.get) {
      val r = exec {
        Option(getViewer).flatMap {
          case v =>
            val (start, end) =
              (v.getTopIndexStartOffset, v.getBottomIndexEndOffset)
            if (end < start) {
              None
            } else Some(Region(ibLength + start, length = end - start))
        }
      }

      r match {
        case Some(r) =>
          Perspective.makePartial(r, overlay)
        case _ =>
          Perspective.makeEmpty(overlay)
      }
    } else {
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

  private[pide] def getSession() = session
}

object Perspective {
  import isabelle.{Text, Document}
  import org.eclipse.jface.text.IRegion
  def makeEmpty(
      overlays : Document.Node.Overlays = Document.Node.Overlays.empty) =
    if (CoqoonUIPreferences.UsePerspective.get) {
      Document.Node.Perspective[Text.Edit, Text.Perspective](true,
          Text.Perspective.empty, overlays)
    } else makeFull(overlays)
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

import dk.itu.coqoon.core.model.{CoqEnforcement, CoqEnforcementContext}

object PIDEEnforcementContext extends CoqEnforcementContext {
  def getSeverity(i : CoqEnforcement.Issue) = i.id match {
    case CoqEnforcement.IsolatedRequire.ID =>
      CoqEnforcement.Severity.Error
    case _ =>
      i.severityHint
  }
}

import dk.itu.coqoon.ui.EventReconciler

private class PIDEReconciler(editor : PIDECoqEditor) extends EventReconciler {
  import EventReconciler._
  import org.eclipse.core.resources.{IMarker,IResource}

  override def process(events : List[DecoratedEvent]) = {
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

import dk.itu.coqoon.core.model.ICoqEntity
import org.eclipse.core.runtime.{Path, IPath}
private class PIDEEntity(
    path : IPath, start : Int, end : Int) extends ICoqEntity {
  /* We get UTF-8 byte offsets out of Coq, and we can't make sense of them
   * until we have an actual editor open on the right buffer... */
  override def open = {
    import dk.itu.coqoon.ui.utilities.UIUtils
    import dk.itu.coqoon.core.utilities.TryAdapt
    import org.eclipse.ui.ide.IDE
    import org.eclipse.core.runtime.Path
    import org.eclipse.core.resources.ResourcesPlugin
    import org.eclipse.core.filesystem.EFS
    import org.eclipse.jface.text.source.ISourceViewer
    val file = EFS.getLocalFileSystem.getStore(path)
    val page =
      UIUtils.getWorkbench.getActiveWorkbenchWindow.getActivePage
    Option(IDE.openInternalEditorOnFileStore(page, file)).
        flatMap(TryAdapt[ISourceViewer]) match {
      case Some(viewer) =>
        import dk.itu.coqoon.ui.text.DocumentAdapter.makeSequence
        import dk.itu.coqoon.ui.utilities.OffsetCorrection.{
          utf8OffsetToCharOffset => fix}
        val seq = makeSequence(viewer.getDocument)
        (fix(start, seq), fix(end, seq)) match {
          case (Some(start), Some(end)) =>
            viewer.revealRange(start, end)
            viewer.setSelectedRange(start, end)
          case _ =>
        }
      case _ =>
    }
  }
}
private object PIDEEntity {
  import isabelle.XML._
  def makeModelEntity(ibLength : Int, s : isabelle.Document.Snapshot,
      filePath : IPath, e : Tree) : Option[ICoqEntity] = {
    Responses.extractEntity(e) match {
      case Some(Left((file, (start, end)))) =>
        Some(new PIDEEntity(new Path(file), start, end - start))
      case Some(Right((exec_id, (start, end)))) =>
        s.state.find_command(s.version, exec_id) flatMap {
          case (_, command) =>
            s.node.command_start(command).filter(_ >= ibLength).map(
                _ - ibLength).map(offset => {
                val s = offset + start
                val l = end - start
                new PIDEEntity(filePath, s, l)
              })
      }
      case None =>
        None
    }
  }
}