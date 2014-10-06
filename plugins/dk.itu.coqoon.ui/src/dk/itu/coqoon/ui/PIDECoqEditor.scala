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

  private object CommandsLock
  private var commands : Seq[(Int, isabelle.Command)] = Seq()

  session.commands_changed += Session.Consumer[Any]("Coqoon") {
    case changed : Session.Commands_Changed =>
      val snapshot_ = getName.map(
          n => session.snapshot(Document.Node.Name(n)))
      val snapshot = snapshot_.get
      CommandsLock synchronized {
        commands =
          (for (command <- snapshot.node.commands;
               offset <- snapshot.node.command_start(command))
            yield (offset, command)).toSeq
      }
      caretPing()
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

  protected[ui] def getName() : Option[String] =
    getEditorInput match {
      case f : IFileEditorInput =>
        Some(f.getFile.getName)
      case _ => None
    }
  protected[ui] var lastDocument : Option[String] = None
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