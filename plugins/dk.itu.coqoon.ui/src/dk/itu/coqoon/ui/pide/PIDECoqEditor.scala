package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.ui.{
  BaseCoqEditor, CoqGoalsContainer, CoqoonUIPreferences, ManifestIdentifiers}

class PIDECoqEditor extends BaseCoqEditor with CoqGoalsContainer {
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

  def getViewer = super.getSourceViewer

  import isabelle.{XML, Text, Command, Session, Protocol, Document}

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    override def caretMoved(ev : CaretEvent) = caretPing()
  }

  private var lastCommand : Option[isabelle.Command] = None

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  private def caretPing() =
    asyncExec {
      val caret = Option(getViewer).map(_.getTextWidget).filter(
          text => !text.isDisposed).map(_.getCaretOffset)
      val commandResultsAndMarkup = caret.flatMap(caret =>
        CommandsLock synchronized {
          val c = commands.find(
              q => (caret >= q._1 && caret <= (q._1 + q._2.length)))
          lastSnapshot.flatMap(snapshot => c.map(
              c => (c,
                  PIDECoqEditor.extractResults(snapshot, c._2),
                  PIDECoqEditor.extractMarkup(snapshot, c._2))))
        })
      commandResultsAndMarkup match {
        case Some(((offset, command), results, markup)) =>
          val sameCommand = lastCommand.contains(command)

          markup.find(_.name == "goals") match {
            case Some(el)
                if !sameCommand || goals == None =>
              setGoals(PIDECoqEditor.extractGoals(el))
            case Some(el) => /* do nothing? */
            case None => setGoals(None)
          }

          if (!sameCommand) {
            import dk.itu.coqoon.ui.utilities.EclipseConsole
            import isabelle.XML.{Elem, Text}
            for ((_, tree) <- results) tree match {
              case f : Elem
                  if f.name == "writeln_message" &&
                     f.markup.properties.toMap.get("source") != Some("goal") =>
                EclipseConsole.out.println(f.body(0).asInstanceOf[Text].content)
              case f : Elem if f.name == "error_message" =>
                PIDECoqEditor.extractError(f).map(_._2).foreach(
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

  private def commandsUpdated(changed : Seq[Command]) =
    asyncExec {
      val changedResultsAndMarkup =
        CommandsLock synchronized {
          val ls = lastSnapshot.get
          for (c <- changed)
            yield (
                ls.node.command_start(c).map(_ - ibLength), c,
                PIDECoqEditor.extractResults(ls, c),
                PIDECoqEditor.extractMarkup(ls, c))
        }

      val am =
        if (CoqoonUIPreferences.ProcessingAnnotations.get) {
          Option(getDocumentProvider).flatMap(
              p => Option(p.getAnnotationModel(getEditorInput)))
        } else None
      am.foreach(
          model => Option(getViewer).map(_.getDocument).foreach(model.connect))

      import org.eclipse.jface.text.Position
      var toDelete : Seq[(Command, Option[Annotation])] = Seq()
      var annotationsToAdd : Seq[(Command, Annotation, Position)] = Seq()
      var errorsToAdd : Map[Int, ((Command, (Int, Int), String))] = Map()

      try {
        for (i <- changedResultsAndMarkup) i match {
          case (Some(offset), command, results, markup) =>
            val complete =
              Protocol.Status.make(markup.map(_.markup).iterator).is_finished

            /* Extract and display error messages */
            for ((_, tree) <- results) {
              import XML._
              tree match {
                case f : Elem if f.name == "error_message" =>
                  (getFile, PIDECoqEditor.extractError(f)) match {
                    case (Some(f), Some((id, msg, Some(start), Some(end)))) =>
                      errorsToAdd += id -> (command,
                          (offset + start - 1, offset + end - 1), msg)
                    case (Some(f), Some((id, msg, _, _))) =>
                      errorsToAdd += id -> (command,
                          (offset, offset + command.source.length), msg)
                    case _ =>
                  }
                case _ =>
              }
            }
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
          import org.eclipse.jface.text.source.IAnnotationModelExtension
          model match {
            case m : IAnnotationModelExtension =>
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
              m.replaceAnnotations(del.toArray, add)
            case m =>
              for ((command, Some(annotation)) <- toDelete) {
                m.removeAnnotation(annotation)
                annotations -= command
              }
              for ((command, annotation, position) <- annotationsToAdd) {
                m.addAnnotation(annotation, position)
                annotations += (command -> annotation)
              }
          }
          model.disconnect(getViewer.getDocument)
          getSourceViewer.invalidateTextPresentation
        })

        if (!toDelete.isEmpty || !errorsToAdd.isEmpty)
          new UpdateErrorsJob(
              toDelete.map(_._1), errorsToAdd.values.toSeq).schedule
      }

      caretPing
    }

  private object CommandsLock
  private var lastSnapshot : Option[Document.Snapshot] = None
  private var commands : Seq[(Int, isabelle.Command)] = Seq()

  import dk.itu.coqoon.core.debug.CoqoonDebugPreferences

  SessionManager.executeWithSessionLock(session =>
    if (session.phase == Session.Failed) {
      dk.itu.coqoon.ui.utilities.EclipseConsole.err.println(
          session.syslog_content.trim)
    } else {
      session.commands_changed += Session.Consumer[Any]("Coqoon") {
        case changed : Session.Commands_Changed
            if getNodeName.exists(changed.nodes.contains) =>
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
        case _ =>
      }
    })

  import dk.itu.coqoon.core.coqtop.CoqSentence
  import dk.itu.coqoon.core.utilities.TotalReader
  import org.eclipse.ui.{IEditorInput, IFileEditorInput}
  override def doSetInput(input : IEditorInput) = {
    super.doSetInput(input)
    input match {
      case f : IFileEditorInput =>
        val file = f.getFile
        val nodeName = getNodeName.get

        import dk.itu.coqoon.core.model.ICoqModel
        val cp = ICoqModel.toCoqProject(file.getProject)
        val initialisationBlock =
          cp.getLoadPath.map(_.asCommand).mkString("", "\n", "\n")
        ibLength = initialisationBlock.length

        val text = TotalReader.read(file.getContents)
        SessionManager.executeWithSessionLock(_.update(
            Document.Blobs.empty,
            List[Document.Edit_Text](
                nodeName -> Document.Node.Edits(List(
                    Text.Edit.insert(0, initialisationBlock + text))),
                nodeName -> Perspective.createDummy),
            "coq"))
      case _ =>
    }
  }

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.core.resources.IFile
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)
  protected[ui] def getNodeName() =
    getFile.map(file => Document.Node.Name(file.getName))

  protected[ui] var ibLength : Int = 0

  import dk.itu.coqoon.core.utilities.{UniqueRule, JobUtilities}
  import org.eclipse.core.resources.WorkspaceJob
  private class UpdateErrorsJob(
      removed : Seq[Command], added : Seq[(Command, (Int, Int), String)])
      extends WorkspaceJob("Update PIDE markers") {
    setRule(JobUtilities.MultiRule(
        getFile.map(f => f.getWorkspace.getRuleFactory.markerRule(f)).orNull,
        UpdateErrorsJob.rule))
    setSystem(true)

    import org.eclipse.core.runtime.{Status, IProgressMonitor}
    import org.eclipse.core.resources.{IMarker, IResource}
    override def runInWorkspace(monitor : IProgressMonitor) = {
      getFile.foreach(file => {
        import dk.itu.coqoon.core
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
      })
      Status.OK_STATUS
    }
  }
  private object UpdateErrorsJob {
    val rule = new dk.itu.coqoon.core.utilities.UniqueRule
  }
}
object PIDECoqEditor {
  import isabelle._
  import dk.itu.coqoon.core.coqtop.CoqTypes
  private def extractMarkup(snapshot : Document.Snapshot,
      command : isabelle.Command) : Seq[XML.Elem] = {
    val markupTree =
      snapshot.state.command_markup(snapshot.version, command,
          Command.Markup_Index.markup, command.range, Markup.Elements.full)
    (for ((range, entry) <- markupTree.branches;
          markup <- entry.markup)
       yield markup).toSeq
  }
  private def extractResults(snapshot : Document.Snapshot,
      command : isabelle.Command) : Seq[Command.Results.Entry] = {
    val results =
      snapshot.state.command_results(snapshot.version, command)
    results.iterator.toSeq
  }

  import isabelle.XML.{Elem, Text, Tree}
  /* For the time being, we convert exciting new PIDE data into boring old
   * -ideslave-8.4 data, to make it easier to support both at once. */
  private object GoalAssist {
    import dk.itu.coqoon.core.coqtop.CoqTypes
    def extractGoalList(e : Tree) : List[CoqTypes.goal] = e match {
      case e : Elem =>
        e.body.flatMap(extractGoal)
      case _ => List()
    }
    def extractGoal(e : Tree) : Option[CoqTypes.goal] = e match {
      case e : Elem if e.name == "goal" =>
        val propertyMap = e.markup.properties.toMap
        Some(CoqTypes.goal(
            propertyMap.get("id").getOrElse("(unknown)"),
            extractHypotheses(e.body(0)),
            e.body(1).asInstanceOf[Elem].body(0).asInstanceOf[Text].content))
      case _ => None
    }
    import dk.itu.coqoon.core.utilities.TryCast
    def extractHypotheses(e : Tree) : List[String] = e match {
      case e : Elem if e.name == "hypotheses" =>
        for (hypothesis <- e.body.flatMap(TryCast[Elem]);
             text <- TryCast[Text](hypothesis.body(0)))
          yield text.content
      case _ => List()
    }
  }
  private def extractGoals(e : Tree) = e match {
    case e : Elem if e.name == "goals" =>
      Some(CoqTypes.goals(GoalAssist.extractGoalList(e.body(0)), List()))
    case _ => None
  }
  private def extractError(e : Tree) = e match {
    case f : Elem if f.name == "error_message" =>
      val propertyMap = f.markup.properties.toMap
      val (errStart, errEnd) =
        (propertyMap.get("offset").map(Integer.parseInt(_, 10)),
         propertyMap.get("end_offset").map(Integer.parseInt(_, 10)))
      val msg = f.body(0).asInstanceOf[Text].content
      propertyMap.get("id").map(Integer.parseInt(_, 10)).map(
          id => (id, msg, errStart, errEnd))
    case _ => None
  }
}

private object Perspective {
  import isabelle.{Text, Document}
  def createDummy() =
    Document.Node.Perspective[Text.Edit, Text.Perspective](true,
        Text.Perspective.full, Document.Node.Overlays.empty)
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
    editor.getNodeName.foreach(nodeName =>
      SessionManager.executeWithSessionLock(_.update(
          Document.Blobs.empty,
          List[Document.Edit_Text](
              nodeName -> Document.Node.Edits(edits),
              nodeName -> Perspective.createDummy), "coq")))
  }
}