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

object DocumentMonitor extends IPartListener2 with IWindowListener with IDocumentListener with EclipseUtils {
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
      val doc = txt.getDocumentProvider.getDocument(txt.getEditorInput)
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

  import org.eclipse.jface.text.source.{AnnotationPainter, IAnnotationAccess, Annotation, ISourceViewer}
  def maybeInsert (ed : ITextEditor, viewer : ISourceViewer) : Unit = {
    val edi = ed.getEditorInput
    val nam = edi.getName
    val doc = ed.getDocumentProvider.getDocument(edi)
    if (! EclipseTables.DocToProject.contains(doc)) {
      val basename = nam.split("\\.")(0)
      val proj = EclipseTables.StringToProject.getOrElse(basename, { val p = new CoqJavaProject(basename); EclipseTables.StringToProject += basename -> p; Console.println("....instantiated new project...."); p })
      proj.setDocument(doc, nam)

      Console.println("inserted " + nam + " into DocToProject table")
      EclipseTables.DocToProject += doc -> proj

      //install painter!
      val access = new IAnnotationAccess () {
        def getType (ann : Annotation) : Object = ann.getType
        def isMultiLine (ann : Annotation) : Boolean = true
        def isTemporary (ann : Annotation) : Boolean = true
      }
      val painter = new AnnotationPainter(viewer, access)
      painter.addDrawingStrategy("dk.itu.sdg.kopitiam.ProofDrawingStrategy", new ProofDrawingStrategy)
      painter.addAnnotationType("dk.itu.sdg.kopitiam.processed", "dk.itu.sdg.kopitiam.ProofDrawingStrategy")
      painter.addAnnotationType("dk.itu.sdg.kopitiam.processing", "dk.itu.sdg.kopitiam.ProofDrawingStrategy")
      painter.setAnnotationTypeColor("dk.itu.sdg.kopitiam.processed", getPrefColor("coqSentBg"))
      painter.setAnnotationTypeColor("dk.itu.sdg.kopitiam.processing", getPrefColor("coqSentProcessBg"))
      Console.println("installed painter " + painter + " is painting? " + painter.isPaintingAnnotations)
      painter.paint(2) //in order to activate it - better idea?
    }
  }

  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  def activateEditor (ed : IWorkbenchPart) : Unit = {
    Console.println("activated: " + ed)
    if (ed.isInstanceOf[CoqEditor]) {
      val txt = ed.asInstanceOf[CoqEditor]
      maybeInsert(txt, txt.getSource)
      if (DocumentState.activeEditor != ed) {
        ActionDisabler.disableAll
        ActionDisabler.enableStart
      } else
        ActionDisabler.enableMaybe
    } else if (ed.isInstanceOf[JavaEditor]) {
      val txt = ed.asInstanceOf[JavaEditor]
      maybeInsert(txt, txt.getViewer)
      if (JavaPosition.editor != ed)
        JavaPosition.editor = ed.asInstanceOf[JavaEditor]
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
      val doc = txt.getDocumentProvider.getDocument(txt.getEditorInput)
      if (EclipseTables.DocToProject.contains(doc)) {
        EclipseTables.DocToProject(doc).gotClosed(doc)
        EclipseTables.DocToProject.remove(doc)
      }
      if (p == DocumentState.activeEditor) {
        DocumentState.activeEditor = null
        if (CoqOutputDispatcher.goalviewer != null)
          CoqOutputDispatcher.goalviewer.clear
        val initial = DocumentState.positionToShell(0).globalStep
        DocumentState.resetState
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
    if (EclipseTables.DocToProject.contains(doc)) {
      val proj = EclipseTables.DocToProject(doc)
      if (proj.isJava(doc)) {
        proj.coqSource match {
          case Some(d) => Console.println("found coq buffer of same project!")
          case None => Console.println("no coq buffer yet")
          //CoqJavaDocumentProvider.updateCoqCode(coq, java, docstring.substring(0, docstring.indexOf(".java")))
        }
        proj.javaNewerThanSource = true
        //find out whether we modified inside <% or program code!
      }
      if (proj.isCoqModel(doc)) {
        proj.coqSource match {
          case Some(d) => Console.println("found coq source for model you're editing")
          case None => Console.println("no generated coq source yet")
        }
        proj.modelNewerThanSource = true
      }
      if (proj.isCoqSource(doc))
        Console.println("oh noez, someone edited the generated code")
    }
    if (DocumentState.activeDocument == doc) {
      DocumentState._content = None
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
