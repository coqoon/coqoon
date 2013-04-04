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
import org.eclipse.jface.text.IPainter

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

  import org.eclipse.jface.text.{IPainter, ITextViewerExtension4}
  import org.eclipse.jface.text.source.{AnnotationPainter, IAnnotationAccess, Annotation, ISourceViewer}
  def maybeInsert (ed : ITextEditor, viewer : ISourceViewer) : Unit = {
    val edi = ed.getEditorInput
    val nam = edi.getName
    val doc = ed.getDocumentProvider.getDocument(edi)
    if (! EclipseTables.DocToProject.contains(doc)) {
      val basenam = nam.split("\\.")(0)
      val basename =
        if (basenam.endsWith("_model"))
          basenam.substring(0, basenam.size - 6)
        else
          basenam
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
      if (viewer.isInstanceOf[ITextViewerExtension4])
        viewer.asInstanceOf[ITextViewerExtension4].addTextPresentationListener(painter)
      painter.addHighlightAnnotationType("dk.itu.sdg.kopitiam.processed")
      painter.addHighlightAnnotationType("dk.itu.sdg.kopitiam.processing")
      painter.setAnnotationTypeColor("dk.itu.sdg.kopitiam.processed", getPrefColor("coqSentBg"))
      painter.setAnnotationTypeColor("dk.itu.sdg.kopitiam.processing", getPrefColor("coqSentProcessBg"))
      Console.println("installed painter " + painter + " is painting? " + painter.isPaintingAnnotations)
      painter.paint(IPainter.TEXT_CHANGE) //in order to activate it - better idea?
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
    }
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
      if (p == DocumentState.activeEditor || p == JavaPosition.editor)
        () //DocumentState.activeEditor = null
      if (EclipseTables.DocToProject.contains(doc)) {
        EclipseTables.DocToProject(doc).gotClosed(doc)
        EclipseTables.DocToProject.remove(doc)
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
        var foundchange : Boolean = false
        if (proj.proofShell != None) {
          //we're in proof mode and actually care!
          val content = doc.get
          val off = event.getOffset
          val p1 = content.lastIndexOf("<%", off)
          val p2 = content.lastIndexOf("%>", off)
          if (p1 > p2) {
            val p3 = content.indexOf("%>", off)
            val p4 = content.indexOf("<%", off)
            if (p4 > p3 || p4 == -1)
              if (p3 - p1 - 2 > 0) {
                //so, we're inside antiquotes! and we have some positions:
                // <% .. %> .. <% .CHANGE. %> .. <% .. %>
                //       p2    p1          p3    p4
                //strategy is here:
                //locate AST of CHANGE! <- we can be either in proof or spec
                //backtrack to before change
                val nncon = content.drop(p1 + 2).substring(0, p3 - p1 - 2).trim
                if (nncon.startsWith("lvars: ") || nncon.startsWith("precondition: ") || nncon.startsWith("requires: ") ||  nncon.startsWith("postcondition: ") || nncon.startsWith("ensures: ")) {
                  Console.println("it's the spec you changed... (to " + nncon + ")")
                  //change to spec
                  //here we nevertheless have to backtrack (to modelShell)
                  //so we can also re-parse everything

                  //handled below in the !foundchange case - equivalent to
                  //changes to java source code
                } else {
                  Console.println("it's the proof you changed... (to " + nncon + ")")
                  //change to proof script - might need to backtrack
                  //remove proven marker if edit in there...
                  proj.program match {
                    case None => //
                    case Some(p) =>
                      JavaPosition.findMethod(JavaPosition.findASTNode(p, off, 0)) match {
                        case None => //
                        case Some(x) => JavaPosition.unmarkProof(x)
                      }
                  }
                  JavaPosition.cur match {
                    case None => //we're lucky! but how can that happen?
                    case Some(x) =>
                      Console.println("Is " + (x.getStartPosition + x.getLength) + " > " + off + "?")
                      if (x.getStartPosition + x.getLength > off) {
                        //backtrack!
                        //also removes the coqShell props!
                        val css =
                          JavaPosition.getASTbeforeOff(off) match {
                            case Some(bt) =>
                              Console.println("backtracking to " + bt)
                              val csss = bt.getProperty(EclipseJavaASTProperties.coqShell)
                              if (csss != null)
                                //invalidate! everything!
                                Some(csss.asInstanceOf[CoqShellTokens])
                              else
                                None
                            case None =>
                              //start of method...
                              val bla = JavaPosition.method
                              bla match {
                                case Some(x) =>
                                  val csss = x.getProperty(EclipseJavaASTProperties.coqShell)
                                  if (csss != null)
                                    Some(csss.asInstanceOf[CoqShellTokens])
                                  else
                                    None
                                case None => None
                              }
                          }
                        css match {
                          case None =>
                          case Some(x) =>
                            DocumentState.setBusy
                            val cs = CoqTop.dummy
                            CoqTop.writeToCoq("Backtrack " + x.globalStep + " " + x.localStep + " " + (cs.context.length - x.context.length) + ".")
                        }
                      }
                  }
                  proj.ASTdirty = true
                  foundchange = true
                }
              }
          }
        }
        if (!foundchange) {
          //change to actual java code!
          proj.ASTdirty = true
          JavaPosition.unmarkProofs
          proj.proofShell match {
            case None =>
            case Some(x) =>
              JavaPosition.unmark
              JavaPosition.retract
              //CoqRetractAction.doitH
          }
        }
      }
      if (proj.isCoqModel(doc)) {
        Console.println("model updated, setting boolean")
        proj.modelNewerThanSource = true
        JavaPosition.unmarkProofs
        proj.proofShell match {
          case None =>
          case Some(x) =>
            JavaPosition.unmark
            JavaPosition.retract
            //CoqRetractAction.doitH
        }
        DocumentState._content = None
      }
    }
  }

  override def documentAboutToBeChanged (event : DocumentEvent) : Unit = ()
}
