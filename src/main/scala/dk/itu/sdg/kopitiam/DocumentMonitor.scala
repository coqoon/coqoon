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

import org.eclipse.ui.{IPartListener2,IWindowListener}

object DocumentMonitor extends IPartListener2 with IWindowListener with EclipseUtils {
  import org.eclipse.ui.{IWorkbenchPartReference,IWorkbenchWindow,PlatformUI}
  import org.eclipse.jface.text.DocumentEvent

  def init () : Unit = PlatformUI.getWorkbench.getWorkbenchWindows.map(
      _.getPartService.addPartListener(this))

  override def windowActivated (window : IWorkbenchWindow) : Unit =
    { window.getPartService.addPartListener(this) }

  override def partActivated (part : IWorkbenchPartReference) : Unit = ()
  override def partClosed (part : IWorkbenchPartReference) : Unit = { }
  override def partBroughtToTop (part : IWorkbenchPartReference) : Unit = { }
  override def partDeactivated (part : IWorkbenchPartReference) : Unit = { }
  override def partHidden (part : IWorkbenchPartReference) : Unit = { }
  override def partInputChanged (part : IWorkbenchPartReference) : Unit = { }
  override def partOpened (part : IWorkbenchPartReference) : Unit = { }
  override def partVisible (part : IWorkbenchPartReference) : Unit = { }
  override def windowClosed (window : IWorkbenchWindow) : Unit = { }
  override def windowDeactivated (window : IWorkbenchWindow) : Unit = { }
  override def windowOpened (window : IWorkbenchWindow) : Unit = { }

  private def documentChanged (event : DocumentEvent) : Unit = {
    val doc = event.getDocument
    //Console.println("doc " + doc + " changed [@" + event.getOffset + "], len: " + event.getLength)
    if (false) {
      val proj : CoqJavaProject = null
      if (false /* proj.isJava(doc) */) {
        var foundchange : Boolean = false
        if (false /* proj.proofShell != None */) {
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
                      import EclipseJavaHelper.{findMethod, findASTNode}
                      findMethod(findASTNode(p, off, 0)) match {
                        case None => //
                        case Some(x) => //JavaPosition.unmarkProof(x)
                      }
                  }
                  JavaPosition.cur match {
                    case None => //we're lucky! but how can that happen?
                    case Some(x) =>
                      Console.println("Is " + (x.getStartPosition + x.getLength) + " > " + off + "?")
                      if (x.getStartPosition + x.getLength > off) {
                        //backtrack!
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
        }
      }
      if (false /* proj.isCoqModel(doc) */)
        (/* do nothing */)
    }
  }
}
