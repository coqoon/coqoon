/* (c) 2011 Hannes Mehnert */
/* based on http://svn.polarion.org/repos/community/Teamweaver/Teamweaver/trunk/org.teamweaver.context.sensing.eclipse/src/main/java/org/teamweaver/context/sensing/eclipse/document/DocumentMonitor.java , which includes:
* Copyright (c) 2006 - 2009 Technische Universität München (TUM)
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License v1.0
* which accompanies this distribution, and is available at
* http://www.eclipse.org/legal/epl-v10.html
*
* A Monitor to sense manipulation of Documents in Eclispe. 
* An object of this type is able to sense all opened documents as well as the new opened documents
* @author Walid Maalej
*/

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
      if (nam.endsWith(".java") && !EclipseTables.StringToDoc.contains(nam))
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
      if (DocumentState.activeEditor != ed) {
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
      if (p == DocumentState.activeEditor) {
        DocumentState.activeEditor = null
        if (CoqOutputDispatcher.goalviewer != null)
          CoqOutputDispatcher.goalviewer.clear
        val initial =
          if (DocumentState.positionToShell.contains(0))
            DocumentState.positionToShell(0).globalStep
          else {
            Console.println("doitH: using 2 instead of registered position, since there is none")
            2
          }
        DocumentState.positionToShell.clear
        DocumentState.position = 0
        DocumentState.sendlen = 0
        DocumentState.invalidateCoqMarker
        PrintActor.deregister(CoqOutputDispatcher)
        val shell = CoqState.getShell
        DocumentState.setBusy
        CoqTop.writeToCoq("Backtrack " + initial + " 0 " + shell.context.length + ".")
        PrintActor.register(CoqOutputDispatcher)
      }
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
    //Console.println("doc " + doc + " changed [@" + event.getOffset + "], len: " + event.getLength)
    if (EclipseTables.DocToString.contains(doc)) {
      val docstring = EclipseTables.DocToString(doc)
      Console.println("I know it's " + docstring + " you changed")
      if (EclipseTables.StringToDoc.contains(docstring + ".v")) {
        val coq = EclipseTables.StringToDoc(docstring + ".v")
        val java = doc.get
        Console.println("found coq buffer for same file!")
        //CoqJavaDocumentProvider.updateCoqCode(coq, java, docstring.substring(0, docstring.indexOf(".java")))
      }
    }
    if (DocumentState.activeDocument == doc) {
      val txt = DocumentState.content
      val off = event.getOffset
      if (off < DocumentState.position) {
        //retract to before
        Console.println("retracting to " + off + " (from " + DocumentState.position + ")")
        DocumentState.reveal = false
        DocumentState.autoreveal = true
        CoqUndoAction.doitReally(off)
      }
      //also, remove markers around here
      EclipseBoilerPlate.maybeunmark(off)
      //Console.println("may have unmarked stuff")
      ActionDisabler.enableMaybe
    }
  }

  override def documentAboutToBeChanged (event : DocumentEvent) : Unit = ()
}
