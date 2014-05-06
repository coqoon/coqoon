/* (c) 2010-2011 Hannes Mehnert
 * Copyright © 2013 Alexander Faithfull */

package dk.itu.sdg.kopitiam.javap

import dk.itu.coqoon.ui.{
  CoqCommand, CoqEditorHandler, ResourceJob, EditorHandler}
import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.jdt.core.dom.Statement
import org.eclipse.ui.texteditor.ITextEditor

import org.eclipse.core.commands.ExecutionEvent

case class JavaStep(
    val node : Statement,
    override val text : String,
    override val synthetic : Boolean) extends CoqCommand(text, synthetic)

abstract class JavaEditorHandler extends EditorHandler {
  override def editor : ITextEditor = TryCast[ITextEditor](super.editor).orNull

  override def calculateEnabled : Boolean = {
    if (editor == null)
      return false
    val state = getState
    if (state == null || state.busy)
      return false
    return true
  }

  protected def getState = JavaEditorState.requireStateFor(editor)
}
object JavaEditorHandler {
  import scala.collection.mutable.Stack
  def doStepBack(jes : JavaEditorState, f : Stack[JavaStep] => Int) = {
    val p = CoqEditorHandler.getStepBackPair(jes.steps, f)
    if (p._1 > 0) {
      jes.setUnderway(p._2 match {
        case None => None
        case Some(x) => Some(x.node)
      })
      jes.setBusy(true)
      new JavaStepBackJob(jes, p._1).schedule()
    }
  }
}

class VerifyMethodHandler extends JavaEditorHandler {
  import EclipseJavaHelper._
  import org.eclipse.jface.text.ITextSelection
  override def execute (ev : ExecutionEvent) : Object = {
    if (isEnabled()) {
      //plan:
      // a: get project
      val jes = getState
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val bla = getRoot(editor.getEditorInput)
      val cu = getCompilationUnit(bla)
      jes.setCompilationUnit(Some(cu))
      jes.method.map(_ => { jes.setUnderway(None); jes.setMethod(None) })
      // a': CoreJava checking!
      if (CoreJavaChecker.checkAST(jes, cu, doc)) { //no errors!
        // b: if outdated coqString: translate -- need to verify outdated...
        // c: find method and statement we want to prove
        if (walkAST(jes, cu, doc)) { //no errors!
          val off = jes.cursorPosition
          val node = findASTNode(cu, off, 0)
          val md = findMethod(node)
          md match {
            case None => UIUtils.Dialog.information(
                "Cursor not inside of method",
                "Please put the cursor inside of the method to verify")
            case Some(x) =>
              if (jes.completedMethods.contains(x))
                UIUtils.Dialog.warning("Already proven",
                    "Sorry, this method was already proven")
              else {
                jes.setMethod(Some(x))
                new JavaProofInitialisationJob(jes).schedule
                //proj.proveMethod(x)
              }
          }
        }
      }
    }
    null
  }
}

private object Analysis {
  import org.eclipse.jdt.core.dom.{
    TypeDeclaration, MethodDeclaration, VariableDeclaration}
  import scala.collection.JavaConversions._
  def getProgramVariables(m : MethodDeclaration) = {
    var names : Seq[VariableDeclaration] = m.parameters.flatMap(
        TryCast[VariableDeclaration]).toSeq
    var parent = Option(m.getParent)
    while (parent != None) parent match {
      case Some(p : TypeDeclaration) =>
        names ++= p.getFields.flatMap[Any, Seq[Any]](
            _.fragments).flatMap(TryCast[VariableDeclaration])
        parent = Option(p.getParent)
      case Some(p) =>
        parent = Option(p.getParent)
      case _ =>
    }
    names
  }
}

import org.eclipse.core.resources.ResourcesPlugin

class SaveProofCertificateHandler extends JavaEditorHandler {
  import SaveProofCertificateHandler._
  override def execute(ev : ExecutionEvent) = {
    import org.eclipse.ui.ide.undo.{WorkspaceUndoUtil, CreateFileOperation}
    import org.eclipse.ui.dialogs.SaveAsDialog
    if (isEnabled()) {
      val jes = getState
      val shell = UIUtils.getActiveShell
      val d = new org.eclipse.ui.dialogs.SaveAsDialog(shell)
      d.setOriginalFile(getCertificateFile(jes))
      if (d.open() == org.eclipse.jface.window.Window.OK) {
        val ws = ResourcesPlugin.getWorkspace.getRoot
        val file = ws.getFile(d.getResult)
        val contents = new java.io.ByteArrayInputStream(
          jes.createCertificate.getBytes(
            java.nio.charset.Charset.forName("UTF-8")))
        val monitor = UIUtils.getProgressMonitor(jes.editor)
        if (!file.exists) {
          UIUtils.getWorkbench.getOperationSupport.getOperationHistory.execute(
            new CreateFileOperation(
              ws.getFile(d.getResult), null, contents,
              "Saving proof certificate"),
            monitor, WorkspaceUndoUtil.getUIInfoAdapter(shell))
        } else {
          import org.eclipse.core.runtime.{
            Status, CoreException, IProgressMonitor}
          import org.eclipse.core.resources.{IResource, WorkspaceJob}
          new ResourceJob("Saving proof certificate", file,
              ws.getWorkspace.getRuleFactory.modifyRule) {
            override def doOperation(monitor : IProgressMonitor) =
              file.setContents(contents, IResource.KEEP_HISTORY, monitor)
          }.schedule
        }
      }
    }
    null
  }

  override def calculateEnabled = if (super.calculateEnabled) {
    val state = getState
    val cu = state.compilationUnit
    if (cu == None) {
      false
    } else state.completedMethods.size == JavaASTUtils.countMethods(cu.get)
  } else false
}
private object SaveProofCertificateHandler {
  def getCertificateFile(jes : JavaEditorState) =
    ResourcesPlugin.getWorkspace.getRoot.getFile(jes.file.
        get.getFullPath.removeFileExtension.addFileExtension("cert.v"))
}

class JavaStepForwardHandler extends JavaEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val jes = getState
      JavaStepForwardHandler.collectProofScript(jes, false) match {
        case a : List[JavaStep] if a.size == 1 =>
          jes.setUnderway(Some(a.last.node))
          scheduleJob(new JavaStepForwardJob(a, jes))
        case _ =>
      }
    }
    null
  }
}
object JavaStepForwardHandler {
  def collectProofScript(
      jes : JavaEditorState, multiple : Boolean, limit : Option[Int] = None) :
      List[JavaStep] =
    collectProofScript(jes.method.get, multiple,
        jes.complete.map { _.getStartPosition }, limit)

  import org.eclipse.jdt.core.dom.MethodDeclaration
  def collectProofScript(
      method : MethodDeclaration, multiple : Boolean,
      start : Option[Int], end : Option[Int]) : List[JavaStep] = {
    val captureP : (Statement => Boolean) = (start, end) match {
      case (Some(a), Some(b)) =>
        (c => c.getStartPosition > a &&
            (c.getStartPosition + c.getLength <= b))
      case (Some(a), None) =>
        (c => c.getStartPosition > a)
      case (None, Some(b)) =>
        (c => c.getStartPosition + c.getLength <= b)
      case (None, None) =>
        (_ => true)
    }
    def print(x : Statement) : Option[JavaStep] =
      if (captureP(x)) {
        JavaASTUtils.printProofScript(x).headOption.map(
            a => JavaStep(x, a._1.toString, a._2))
      } else None

    JavaASTUtils.traverseAST(method, true, !multiple, print)
  }
}

class JavaStepAllHandler extends JavaEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val jes = getState
      JavaStepForwardHandler.collectProofScript(jes, true) match {
        case a : List[JavaStep] if a.size > 0 =>
          jes.setUnderway(Some(a.last.node))
          scheduleJob(new JavaStepForwardJob(a, jes))
        case _ =>
      }
    }
    null
  }
}

class JavaStepToCursorHandler extends JavaEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val jes = getState
      val underwayPos =
        jes.underway.map(a => a.getStartPosition + a.getLength) getOrElse 0
      val cursorPos = jes.cursorPosition
      if (cursorPos > underwayPos) { /* Forwards! */
        JavaStepForwardHandler.collectProofScript(
            jes, true, Some(jes.cursorPosition)) match {
          case a : List[JavaStep] if a.size > 0 =>
            jes.setUnderway(Some(a.last.node))
            scheduleJob(new JavaStepForwardJob(a, jes))
          case _ =>
        }
      } else if (cursorPos < underwayPos) { /* Backwards! */
        JavaEditorHandler.doStepBack(jes, _.prefixLength(
            a => (cursorPos < (a.node.getStartPosition + a.node.getLength))))
      }
    }
    null
  }
}

class JavaStepBackHandler extends JavaEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      JavaEditorHandler.doStepBack(getState, a => if (a.length > 0) 1 else 0)
    null
  }

  override def calculateEnabled =
    super.calculateEnabled && (getState.steps.length > 0)
}

class JavaRetractAllHandler extends JavaEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      JavaEditorHandler.doStepBack(getState, _.length)
    null
  }

  override def calculateEnabled =
    super.calculateEnabled && (getState.steps.length > 0)
}
