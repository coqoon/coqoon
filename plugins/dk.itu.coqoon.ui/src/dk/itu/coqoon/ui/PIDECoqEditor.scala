package dk.itu.coqoon.ui

class PIDECoqEditor extends BaseCoqEditor {
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
  protected[ui] val syntax = new Coq_Syntax
  protected[ui] val resources = new Coq_Resources(syntax)
  protected[ui] val session = new Session(resources)

  import org.eclipse.swt.custom.{CaretEvent, CaretListener}
  object DocumentCaretListener extends CaretListener {
    override def caretMoved(ev : CaretEvent) = caretPing()
  }

  import dk.itu.coqoon.ui.utilities.UIUtils.asyncExec
  private def caretPing() =
    asyncExec {
      val offset = getViewer.getTextWidget.getCaretOffset
      val selectedCommand = CommandsLock synchronized {
        commands.find(
            q => (offset >= q._1 && offset <= (q._1 + q._2.length)))
      }
      println(selectedCommand)
    }

  private def commandsUpdated() =
    asyncExec {
      /* Clear all old error messages */
      import dk.itu.coqoon.core
      import org.eclipse.core.resources.IResource
      getFile.foreach(file => new DeleteMarkersJob(file,
          core.ManifestIdentifiers.MARKER_PROBLEM, true,
          IResource.DEPTH_ZERO).schedule)

      val allResults =
        CommandsLock synchronized {
          for ((offset, i) <- commands)
            yield (offset, i, PIDECoqEditor.extractResults(lastSnapshot.get, i))
        }
      /* Extract and display error messages */
      for ((offset, command, results) <- allResults;
           (_, tree) <- results) {
        import XML._
        tree match {
          case f : Elem if f.name == "error_message" =>
            val propertyMap = f.markup.properties.toMap
            val (errStart, errEnd) =
              (propertyMap.get("offset").map(Integer.parseInt(_, 10)),
               propertyMap.get("end_offset").map(Integer.parseInt(_, 10)))
            val msg = f.body(0).asInstanceOf[Text].content
            println(errStart, errEnd, msg)
            (getFile, errStart, errEnd) match {
              case (Some(f), Some(start), Some(end)) =>
                CreateErrorMarkerJob(f,
                    (offset + start - 1, offset + end - 1),
                    msg).schedule
              case (Some(f), _, _) =>
                CreateErrorMarkerJob(f,
                    (offset, offset + command.source.length), msg).schedule
              case _ =>
            }
          case _ =>
        }
      }

      caretPing
    }

  private object CommandsLock
  private var lastSnapshot : Option[Document.Snapshot] = None
  private var commands : Seq[(Int, isabelle.Command)] = Seq()

  session.commands_changed += Session.Consumer[Any]("Coqoon") {
    case changed : Session.Commands_Changed =>
      CommandsLock synchronized {
        lastSnapshot = getName.map(
            n => session.snapshot(Document.Node.Name(n)))
        lastSnapshot.foreach(snapshot =>
          commands =
            (for (command <- snapshot.node.commands;
                 offset <- snapshot.node.command_start(command))
              yield (offset, command)).toSeq)
      }
      commandsUpdated()
    case _ =>
  }

  session.start("Coq", Nil)

  while (!session.is_ready && session.phase != Session.Failed)
    Thread.sleep(500)
  if (session.phase == Session.Failed)
    println("Oh, no")

  override def dispose = {
    session.stop
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
        import dk.itu.coqoon.core
        val text = TotalReader.read(file.getContents)
        lastDocument = Option(text)
        session.update(
            Document.Blobs.empty,
            List[Document.Edit_Text](
                Document.Node.Name(file.getName) ->
                    Document.Node.Edits(List(
                        Text.Edit.insert(0, text)))))
      case _ =>
    }
  }

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.core.resources.IFile
  protected[ui] def getFile() : Option[IFile] =
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile)
  protected[ui] def getName() : Option[String] =
    getFile.map(_.getName)
  protected[ui] var lastDocument : Option[String] = None
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

    try {
      val edits : Document.Node.Edits[Text.Edit, Text.Perspective] =
          dr.getType match {
        case DirtyRegion.INSERT =>
          Document.Node.Edits(List(Text.Edit.insert(dr.getOffset, dr.getText)))
        case DirtyRegion.REMOVE =>
          /* The region doesn't actually carry the deleted text, so retrieve it
           * from the last known state of the document */
          Document.Node.Edits(List(Text.Edit.remove(dr.getOffset,
              editor.lastDocument.get.substring(
                  dr.getOffset, dr.getOffset + dr.getLength))))
      }
      editor.getName.foreach(name => editor.session.update(
          Document.Blobs.empty,
          List[Document.Edit_Text](Document.Node.Name(name) -> edits)))
    } finally editor.lastDocument = Option(editor.getViewer.getDocument.get)
  }

  private var doc : Option[IDocument] = None
  override def setDocument(newDocument : IDocument) =
    doc = Option(newDocument)
}