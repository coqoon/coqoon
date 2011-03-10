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
      //add java files to doc2str table - but not the ones automatically translated
      //to Coq code by Kopitiam (present in str2doc table)
      if (nam.endsWith(".java") &&
          (! EclipseTables.StringToDoc.contains(nam) || EclipseTables.StringToDoc(nam) != doc))
        EclipseTables.DocToString += doc -> nam
      doc.addDocumentListener(this)
    }
  }

  def init () : Unit = {
    val wins = PlatformUI.getWorkbench.getWorkbenchWindows
    wins.map(x => x.getPartService.addPartListener(this))
    wins.map(x => x.getPages.toList.map(y => y.getEditorReferences.toList.map(z => handlePart(z.getEditor(false)))))
  }


  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.{IResource, IMarker}
  var activeeditor : CoqEditor = null
  override def partActivated (part : IWorkbenchPartReference) : Unit = {
    val ed = part.getPart(false)
    if (ed.isInstanceOf[CoqEditor])
      if (activeeditor != ed) {
        //Console.println("part activated " + ed)
        if (activeeditor != null)
          activeeditor.getEditorInput.asInstanceOf[IFileEditorInput].getFile.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
        activeeditor = ed.asInstanceOf[CoqEditor]
        ActionDisabler.disableAll
        DocumentState.position = 0
        DocumentState.sendlen = 0
        DocumentState.undoAll
        DocumentState.sourceview = activeeditor.getSource
        DocumentState.totallen = EclipseBoilerPlate.getContent.length
        PrintActor.callbacks = List(CoqOutputDispatcher)
        if (! CoqTop.isStarted) {
          CoqStartUp.start()
        } else {
          val shell = CoqState.getShell
          PrintActor.register(CoqStartUp)
          CoqTop.writeToCoq("Backtrack " + DocumentState.coqstart + " 0 " + shell.context.length + ".")
          ActionDisabler.enableMaybe
        }
      } else
        ActionDisabler.enableMaybe
    else
      ActionDisabler.disableAll
        //Console.println("didn't expect to come here, activation of an already activated editor")
  }

  override def partOpened (part : IWorkbenchPartReference) : Unit = {
    handlePartRef(part)
  }

  override def windowActivated (window : IWorkbenchWindow) : Unit = {
    window.getPartService.addPartListener(this)
  }

  override def partClosed (part : IWorkbenchPartReference) : Unit = {
    val p = part.getPart(false)
    if (p.isInstanceOf[ITextEditor]) {
      val txt = p.asInstanceOf[ITextEditor]
      val ed = txt.getEditorInput
      val nam = ed.getName
      val doc = txt.getDocumentProvider.getDocument(ed)
      if (EclipseTables.DocToString.contains(doc))
        EclipseTables.DocToString.remove(doc)
      if (EclipseTables.StringToDoc.contains(nam))
        EclipseTables.StringToDoc.remove(nam)
    }
  }
  override def partBroughtToTop (part : IWorkbenchPartReference) : Unit = { }
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
