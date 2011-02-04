/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.jface.text.IDocumentListener
import org.eclipse.ui.{IPartListener2,IWindowListener}

object DocumentMonitor extends IPartListener2 with IWindowListener with IDocumentListener {
  import org.eclipse.ui.{IWorkbenchPartReference,IWorkbenchPart,IWorkbenchWindow,PlatformUI}
  import org.eclipse.ui.texteditor.ITextEditor
  import org.eclipse.jface.text.DocumentEvent

  import org.eclipse.jface.text.IDocument

  def handlePartRef (p : IWorkbenchPartReference) : Unit = {
    val t = p.getPart(false)
    handlePart(t)
  }

  def handlePart (t : IWorkbenchPart) : Unit = {
    if (t.isInstanceOf[ITextEditor]) {
      val txt = t.asInstanceOf[ITextEditor]
      val ed = txt.getEditorInput
      val doc = txt.getDocumentProvider.getDocument(ed)
      val nam = ed.getName
      if (nam.endsWith(".java") && !doc.isInstanceOf[CoqDocument]) //HACK: coqdocument should end with .v
        EclipseTables.DocToString += doc -> ed.getName
      doc.addDocumentListener(this)
    }
  }

  def init () : Unit = {
    Console.println("initializing DocumentMonitor")
    val wins = PlatformUI.getWorkbench.getWorkbenchWindows
    wins.map(x => x.getPartService.addPartListener(this))
    wins.map(x => x.getPages.toList.map(y => y.getEditorReferences.toList.map(z => handlePart(z.getEditor(false)))))
  }

  override def partActivated (part : IWorkbenchPartReference) : Unit = {
    handlePartRef(part)
  }

  override def partOpened (part : IWorkbenchPartReference) : Unit = {
    handlePartRef(part)
  }

  override def windowActivated (window : IWorkbenchWindow) : Unit = {
    window.getPartService.addPartListener(this)
  }

  override def partBroughtToTop (part : IWorkbenchPartReference) : Unit = { }
  override def partClosed (part : IWorkbenchPartReference) : Unit = { }
  override def partDeactivated (part : IWorkbenchPartReference) : Unit = { }
  override def partHidden (part : IWorkbenchPartReference) : Unit = { }
  override def partInputChanged (part : IWorkbenchPartReference) : Unit = { }
  override def partVisible (part : IWorkbenchPartReference) : Unit = { }
  override def windowClosed (window : IWorkbenchWindow) : Unit = { }
  override def windowDeactivated (window : IWorkbenchWindow) : Unit = { }
  override def windowOpened (window : IWorkbenchWindow) : Unit = { }
  override def documentChanged (event : DocumentEvent) : Unit = {
    val doc = event.getDocument
    Console.println("doc " + doc + " changed [@" + event.getOffset + "]: " + event.getText)
    if (EclipseTables.DocToString.contains(doc)) {
      val docstring = EclipseTables.DocToString(doc)
      Console.println("I know it's " + docstring + " you changed")
      if (EclipseTables.StringToDoc.contains(docstring)) {
        val coq = EclipseTables.StringToDoc(docstring)
        val java = doc.get
        Console.println("found coq buffer for same file!")
        CoqJavaDocumentProvider.up(coq, java)
      }
    }
  }
  override def documentAboutToBeChanged (event : DocumentEvent) : Unit = { }
}
