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
    DocumentState.activated = ed
    if (ed.isInstanceOf[CoqEditor]) {
      val txt = ed.asInstanceOf[CoqEditor]
      maybeInsert(txt, txt.getSource)
    } else if (ed.isInstanceOf[JavaEditor]) {
      val txt = ed.asInstanceOf[JavaEditor]
      maybeInsert(txt, txt.getViewer)
      if (JavaPosition.editor != ed)
        JavaPosition.editor = ed.asInstanceOf[JavaEditor]
    }
    ActionDisabler.disableAll
    ActionDisabler.enableMaybe
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
        JavaPosition.retract
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
      DocumentState._content = None
      if (proj.isJava(doc)) {
        val oldval = proj.javaNewerThanSource
        proj.javaNewerThanSource = true
        if (proj.getCoqString != None) {
          val content = doc.get
          val off = event.getOffset
          val p1 = content.lastIndexOf("<%", off)
          val p2 = content.lastIndexOf("%>", off)
          if (p1 > p2) {
            val p3 = content.indexOf("%>", off)
            val p4 = content.indexOf("<%", off)
            if (p4 > p3 || p4 == -1) {
              var found : Boolean = false
              var rtr : Boolean = false
              val l = doc.getLineOfOffset(p1) + 1
              var offc : Pair[Int, Int] = (0, 0)
              var offintocoq : Int = 0
              val nncon = content.drop(p1 + 2).substring(0, p3 - p1 - 2).trim
              var name : Option[String] = None
              //Console.println("new content is: " + nncon)
              if (nncon.startsWith("lvars: ") || nncon.startsWith("precondition: ") || nncon.startsWith("requires: ") ||  nncon.startsWith("postcondition: ") || nncon.startsWith("ensures: ")) {
                for (x <- proj.specOffsets.keys) {
                  var i : Int = 0
                  val coqoffs = proj.specOffsets(x)._2
                  for (y <- proj.specOffsets(x)._1) {
                    if (y.line == l) { //gotcha!
                      found = true
                      val oldc = proj.getCoqString.getOrElse(" ")
                      val nnc = nncon.substring(nncon.indexOf(":") + 1).trim
                      val nc = 
                        if (nncon.startsWith("lvars: ")) {
                          val spl = nnc.split(",")
                          if (spl.length == 0)
                            ""
                          else
                            spl.mkString("[A] ", ", [A]", "")
                        } else
                          nnc
                      //Console.println("setting new content to be " + nc)
                      val newc = oldc.take(proj.specOffset + coqoffs._1 + coqoffs._2(i)._1) + nc + oldc.drop(proj.specOffset + coqoffs._1 + coqoffs._2(i)._1 + coqoffs._2(i)._2)
                      //Console.println("new content is " + newc.drop(proj.specOffset + coqoffs._1 + coqoffs._2(i)._1 - 10).take(50))
                      proj.setCoqString(Some(newc))
                      DocumentState._content = Some(newc)
                      offc = (coqoffs._1 + coqoffs._2(i)._1, nc.length - coqoffs._2(i)._2)
                      offintocoq = coqoffs._1 + coqoffs._2(i)._1 + proj.specOffset
                      name = Some(x)
                      rtr = true
                    }
                    i = i + 1
                  }
                }
                proj.updateSpecOffsets(offc, name)
              } else {
                var name : Option[String] = None
                for (x <- proj.javaOffsets.keys) {
                  var i : Int = 0
                  for (p <- proj.javaOffsets(x)._2) {
                    //Console.println("checking " + l + " against " + p.line + " in " + x)
                    if (l == p.line) {
                      found = true
                      Console.println("found something! excited! " + x + " i is " + i)
                      val coqp = proj.coqOffsets(x)._2(i)
                      val coqoff = coqp._1 + proj.coqOffsets(x)._1
                      val ncon =
                        if (nncon.startsWith("invariant")) {
                          val vals = nncon.drop(10).trim.split("frame:")
                          assert(vals.length == 2)
                          "forward (" + vals(0).trim + ") (" + vals(1).trim + ")."
                        } else
                          nncon
                      Console.println("new content: " + ncon)
                      val oldc = proj.methods(x)
                      val newc = oldc.take(coqoff) + ncon + oldc.drop(coqoff + coqp._2)
                      Console.println("new coq buffer: " + newc.drop(scala.math.max(0, coqoff - 10)).take(scala.math.min(coqp._2 + 20, newc.length)))
                      offc = (proj.coqOffsets(x)._1 + coqp._1, ncon.length - coqp._2)
                      offintocoq = proj.coqOffsets(x)._1 + coqp._1 + proj.proofOffset
                      name = Some(x)
                      proj.methods = proj.methods + (x -> newc)
                      DocumentState._content = proj.getCoqString
                      if (JavaPosition.name == x) rtr = true
                      //DocumentState._content = Some(newc)
                      //update the javaOffsets table (only if newline)
                      //if there's a file or editor, rewrite that as well!
                      // -> maybe do that on ctrl + s in the java buffer?!?
                      //we might need to backtrack in coq + java!
                    }
                    i = i + 1
                  }
                }
                proj.updateCoqOffsets(offc, name)
              }
              if (found && ! oldval) {
                Console.println("javaNewerThanSource is false again")
                proj.javaNewerThanSource = false
                //retract! XXXX
                if (rtr && offintocoq < DocumentState.position)
                  CoqUndoAction.doitReally(offintocoq)
              }
            }
          }
        }
        ActionDisabler.enableMaybe
      }
      if (proj.isCoqModel(doc)) {
        Console.println("model updated, setting boolean")
        proj.modelNewerThanSource = true
        DocumentState._content = None
      }
      if (proj.isCoqSource(doc))
        Console.println("oh noez, someone edited the generated code")
    }
    if (DocumentState.activeDocument == doc) {
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
      DocumentState._content = None
    }
  }

  override def documentAboutToBeChanged (event : DocumentEvent) : Unit = ()
}
