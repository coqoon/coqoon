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
    val wb = PlatformUI.getWorkbench
    wb.getDisplay.asyncExec(new Runnable() {
      def run () = {
        val win = wb.getActiveWorkbenchWindow
        assert(win != null)
        EclipseBoilerPlate.window = win
        activateEditor(win.getActivePage.getActiveEditor)
      }
    })
  }


  override def partActivated (part : IWorkbenchPartReference) : Unit = {
    val ed = part.getPart(false)
    activateEditor(ed)
  }

  var activeeditor : CoqEditor = null
  def activateEditor (ed : IWorkbenchPart) : Unit = {
    Console.println("activated: " + ed)
    if (ed.isInstanceOf[CoqEditor]) {
      val txt = ed.asInstanceOf[CoqEditor]
      val edi = txt.getEditorInput
      val nam = edi.getName
      val doc = txt.getDocumentProvider.getDocument(edi)
      if (! EclipseTables.StringToDoc.contains(nam)) {
        Console.println("inserted " + nam + " into String2Doc table")
        EclipseTables.StringToDoc += nam -> doc
      }
      if (activeeditor != ed) {
        //Console.println("part activated " + ed)
        ActionDisabler.disableAll
        ActionDisabler.enableStart
      } else
        ActionDisabler.enableMaybe
    } else
      ActionDisabler.disableAll
  }

  override def partOpened (part : IWorkbenchPartReference) : Unit =
    { handlePartRef(part) }

  override def windowActivated (window : IWorkbenchWindow) : Unit =
    { window.getPartService.addPartListener(this) }

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
      if (EclipseTables.StringToDoc.contains(docstring + ".v")) {
        val coq = EclipseTables.StringToDoc(docstring + ".v")
        val java = doc.get
        Console.println("found coq buffer for same file!")
        CoqJavaDocumentProvider.up(coq, java, docstring.substring(0, docstring.indexOf(".java")))
      }
    }
    //more along the lines as (but doesn't work since EclipseBoilerPlate depends on getActivePage and getCaretPosition)
    //val adoc = activeeditor.getDocumentProvider.getDocument(activeeditor.getEditorInput)      if (adoc == doc) {
    if (activeeditor != null && activeeditor == PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage.getActiveEditor) {
      val txt = doc.get
      val len = event.getLength
      val off = event.getOffset
      DocumentState.totallen += (event.getText.length - len)
      if (off < DocumentState.position) {
        DocumentState.position = scala.math.max(DocumentState.position, DocumentState.totallen - 1)
        //retract to before
        Console.println("retracting to " + off + " (from " + DocumentState.position + ")")
        CoqStepUntilAction.doit
      }
      val con = doc.get.take(off + len).drop(off)
      Console.println("in active coq buffer, replace :" + con + ": with :" + event.getText + ":")
      //also, remove markers around here
      Console.println("upgraded totallen with " + (event.getText.length - len))
      EclipseBoilerPlate.maybeunmark(off)
      Console.println("may have unmarked stuff")
      ActionDisabler.enableMaybe
    }
  }
  override def documentAboutToBeChanged (event : DocumentEvent) : Unit = { }
}
