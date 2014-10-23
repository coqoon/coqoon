package dk.itu.coqoon.ui

class PIDECoqEditor extends BaseCoqEditor with CoqGoalsContainer {
  import org.eclipse.jface.text.reconciler.MonoReconciler
  private val reconciler =
    new MonoReconciler(new PIDEReconcilingStrategy(this), true)
  reconciler.setDelay(200)

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

  import isabelle._
  import dk.itu.coqoon.core.CoqoonPreferences
  protected[ui] val syntax = new Coq_Syntax
  protected[ui] val resources = new Coq_Resources(syntax,
      CoqoonPreferences.CoqPath.get match {
        case Some(path) => path + java.io.File.separator + "coqtop"
        case _ => "coqtop"
      })
  protected[ui] val session = new Session
  session.register_resources("coq", resources)

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    override def caretMoved(ev : CaretEvent) = caretPing()
  }

  private var lastCommand : Option[isabelle.Command] = None

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  private def caretPing() =
    asyncExec {
      val caret = getViewer.getTextWidget.getCaretOffset
      val commandResultsAndMarkup = CommandsLock synchronized {
        val c = commands.find(
            q => (caret >= q._1 && caret <= (q._1 + q._2.length)))
        lastSnapshot.flatMap(snapshot => c.map(
            c => (c,
                PIDECoqEditor.extractResults(snapshot, c._2),
                PIDECoqEditor.extractMarkup(snapshot, c._2))))
      }
      commandResultsAndMarkup match {
        case Some(((_, command), _, _))
            if lastCommand.contains(command) =>
          /* do nothing */
        case Some(((offset, command), results, markup)) =>
          markup.find(_.name == "goals") match {
            case Some(el) =>
              setGoals(PIDECoqEditor.extractGoals(el))
            case _ =>
              setGoals(None)
          }

          import dk.itu.coqoon.ui.utilities.EclipseConsole
          import isabelle.XML.{Elem, Text}
          for ((_, tree) <- results) tree match {
            case f : Elem if f.name == "writeln_message" =>
              EclipseConsole.out.println(f.body(0).asInstanceOf[Text].content)
            case f : Elem if f.name == "error_message" =>
              PIDECoqEditor.extractError(f).map(_._1).foreach(
                  EclipseConsole.err.println)
            case _ =>
          }
          lastCommand = Option(command)
        case _ =>
          setGoals(None)
          lastCommand = None
      }
    }

  private def commandsUpdated() =
    asyncExec {
      /* Clear all old error messages */
      import dk.itu.coqoon.core
      import org.eclipse.core.resources.IResource
      getFile.foreach(file => new DeleteMarkersJob(file,
          core.ManifestIdentifiers.MARKER_PROBLEM, true,
          IResource.DEPTH_ZERO).schedule)

      val allResultsAndMarkup =
        CommandsLock synchronized {
          for ((offset, i) <- commands)
            yield (offset, i,
                PIDECoqEditor.extractResults(lastSnapshot.get, i),
                PIDECoqEditor.extractMarkup(lastSnapshot.get, i))
        }

      val am = Option(getDocumentProvider.getAnnotationModel(getEditorInput))
      am.foreach(model => {
        model.connect(getViewer.getDocument)
        var t = model.getAnnotationIterator
        while (t.hasNext)
          model.removeAnnotation(t.next.asInstanceOf[
            org.eclipse.jface.text.source.Annotation])
      })
      try {
        for ((offset, command, results, markup) <- allResultsAndMarkup) {
          var complete =
            Protocol.Status.make(markup.map(_.markup).iterator).is_finished

          /* Extract and display error messages */
          for ((_, tree) <- results) {
            import XML._
            tree match {
              case f : Elem if f.name == "error_message" =>
                (getFile, PIDECoqEditor.extractError(f)) match {
                  case (Some(f), Some((msg, Some(start), Some(end)))) =>
                    CreateErrorMarkerJob(f,
                        (offset + start - 1, offset + end - 1),
                        msg).schedule
                  case (Some(f), Some((msg, _, _))) =>
                    CreateErrorMarkerJob(f,
                        (offset, offset + command.source.length), msg).schedule
                  case _ =>
                }
              case _ =>
            }
          }
          am.foreach(model => {
            import org.eclipse.jface.text.Position
            import org.eclipse.jface.text.source.Annotation
            if (!complete)
              model.addAnnotation(
                new Annotation(
                    ManifestIdentifiers.ANNOTATION_PROCESSING,
                    false, "Processing proof"),
                new Position(offset, command.source.length))
          })
        }
      } finally am.foreach(model => {
        model.disconnect(getViewer.getDocument)
        getSourceViewer.invalidateTextPresentation
      })

      caretPing
    }

  private object CommandsLock
  private var lastSnapshot : Option[Document.Snapshot] = None
  private var commands : Seq[(Int, isabelle.Command)] = Seq()

  import dk.itu.coqoon.core.CoqoonPreferences.PrintIdeslaveTraffic
  session.commands_changed += Session.Consumer[Any]("Coqoon") {
    case changed : Session.Commands_Changed =>
      if (PrintIdeslaveTraffic.get)
        println(s"! ${changed}")
      CommandsLock synchronized {
        lastSnapshot = getName.map(
            n => session.snapshot(Document.Node.Name(n)))
        lastSnapshot.foreach(snapshot =>
          commands =
            (for (command <- snapshot.node.commands;
                  offset <- snapshot.node.command_start(command)
                    if offset >= ibLength)
              yield (offset - ibLength, command)).toSeq)
      }
      commandsUpdated()
    case q =>
      if (PrintIdeslaveTraffic.get)
        println(s"! ${q}")
  }
  session.all_messages += Session.Consumer("Coqoon")(q =>
    if (PrintIdeslaveTraffic.get)
      println(s"? ${q}"))

  session.start("coq", Nil)

  while (!session.is_ready && session.phase != Session.Failed)
    Thread.sleep(500)
  if (session.phase == Session.Failed)
    println("Oh, no")

  override def dispose = {
    session.stop("coq")
    super.dispose
  }

  import dk.itu.coqoon.core.coqtop.CoqSentence
  import dk.itu.coqoon.core.utilities.TotalReader
  import org.eclipse.ui.{IEditorInput, IFileEditorInput}
  override def doSetInput(input : IEditorInput) = {
    super.doSetInput(input)
    input match {
      case f : IFileEditorInput =>
        val file = f.getFile

        import dk.itu.coqoon.core.model.ICoqModel
        val cp = ICoqModel.toCoqProject(file.getProject)
        val initialisationBlock =
          cp.getLoadPath.map(_.asCommand).mkString("", "\n", "\n")
        ibLength = initialisationBlock.length

        val text = TotalReader.read(file.getContents)
        lastDocument = text
        session.update(
            Document.Blobs.empty,
            List[Document.Edit_Text](
                Document.Node.Name(file.getName) ->
                    Document.Node.Edits(List(
                        Text.Edit.insert(0, initialisationBlock + text)))),
            "coq")
      case _ =>
    }
  }

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.core.resources.IFile
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)
  protected[ui] def getName() : Option[String] =
    getFile.map(_.getName)
  protected[ui] var lastDocument : String = ""

  protected[ui] var ibLength : Int = 0
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
      Some(msg, errStart, errEnd)
    case _ => None
  }
}

import org.eclipse.jface.text.reconciler.IReconcilingStrategy

private class PIDEReconcilingStrategy(
    editor : PIDECoqEditor) extends IReconcilingStrategy {
  import org.eclipse.jface.text.{Region, IRegion, IDocument}
  import org.eclipse.jface.text.reconciler.DirtyRegion

  import org.eclipse.core.resources.{IMarker,IResource}

  override def reconcile(r : IRegion) : Unit = ???

  override def reconcile(dr : DirtyRegion, r : IRegion) = {
    import isabelle._

    val edits : Document.Node.Edits[Text.Edit, Text.Perspective] =
        dr.getType match {
      case DirtyRegion.INSERT =>
        val text = dr.getText
        try {
          Document.Node.Edits(List(Text.Edit.insert(
              editor.ibLength + dr.getOffset, text)))
        } finally {
          editor.lastDocument =
              editor.lastDocument.substring(0, dr.getOffset) +
              text +
              (if (dr.getOffset < editor.lastDocument.length)
                 editor.lastDocument.substring(dr.getOffset)
               else "")
        }
      case DirtyRegion.REMOVE =>
        /* The region doesn't actually carry the deleted text, so retrieve it
         * from the last known state of the document */
        try {
          Document.Node.Edits(List(Text.Edit.remove(
              editor.ibLength + dr.getOffset,
              editor.lastDocument.substring(
                  dr.getOffset, dr.getOffset + dr.getLength))))
        } finally {
          editor.lastDocument =
              editor.lastDocument.substring(0, dr.getOffset) +
              (if (dr.getOffset + dr.getLength < editor.lastDocument.length)
                 editor.lastDocument.substring(dr.getOffset + dr.getLength)
               else "")
        }
    }
    editor.getName.foreach(name => editor.session.update(
        Document.Blobs.empty,
        List[Document.Edit_Text](Document.Node.Name(name) -> edits), "coq"))
  }

  private var doc : Option[IDocument] = None
  override def setDocument(newDocument : IDocument) =
    doc = Option(newDocument)
}
