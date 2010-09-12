package coqscala

import org.eclipse.ui.editors.text.TextEditor;

class CoqEditor extends TextEditor {
  import org.eclipse.jface.text.source.ISourceViewer;
	
  def getSource () : ISourceViewer = {
    getSourceViewer();
  }
}

import org.eclipse.ui.IWorkbenchWindowActionDelegate

class CoqStepAction extends IWorkbenchWindowActionDelegate {
  import org.eclipse.ui.{IWorkbenchWindow,IWorkbenchPage,IEditorPart}
  import org.eclipse.ui.texteditor.{ITextEditor,IDocumentProvider,AbstractTextEditor}
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection
  import org.eclipse.jface.text.IDocument

  var window : IWorkbenchWindow = null
  
  override def init (window_ : IWorkbenchWindow) : Unit = {
    window = window_
    Console.println("init called")
  }

  override def run (action : IAction) : Unit = {
    if (! CoqTop.isStarted) {
      CoqTop.startCoq
      if (EclipseConsole.out == null)
        EclipseConsole.initConsole
      PrintActor.stream = EclipseConsole.out
    }
    val editorpart = window.getActivePage.getActiveEditor
    if (editorpart.isInstanceOf[ITextEditor]) {
      if (editorpart.isInstanceOf[CoqEditor]) {
    	val texteditor = editorpart.asInstanceOf[CoqEditor]
        val dp : IDocumentProvider = texteditor.getDocumentProvider
        val doc : IDocument = dp.getDocument(texteditor.getEditorInput)
        val line = doc.getLineOfOffset(DocumentState.position)
        val length = doc.getLineLength(line)
        val content = doc.get(DocumentState.position, length)
        DocumentState.sendlen = length
        DocumentState.sourceview = texteditor.getSource //should only be called once, somehow!
        Console.println("content is " + content)
        CoqTop.writeToCoq(content)
      } else {
        Console.println("not a CoqEditor!")
      }
    } else
    	Console.println("not a ITextEditor!")
  }

  override def selectionChanged (action : IAction, selection : ISelection) : Unit = { }

  override def dispose () : Unit = { }	
}

object CoqStepAction extends CoqStepAction {}

object DocumentState {
  import org.eclipse.jface.text.{ITextViewer}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display
 
  var sourceview : ITextViewer = null
  var position : Int = 0
  var sendlen : Int = 0

  def commit () : Unit = {
    val bl = new Color(Display.getDefault, new RGB(0, 0, 220))
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = sourceview.setTextColor(bl, position, sendlen, false)
    });

    position += sendlen
    sendlen = 0
  }
}

object EclipseConsole {
  import org.eclipse.ui.console.{MessageConsole,MessageConsoleStream,IConsole,IConsoleManager,ConsolePlugin}
  var out : MessageConsoleStream = null

  def initConsole () : Unit = {
    val conman : IConsoleManager = ConsolePlugin.getDefault.getConsoleManager
    val existing = conman.getConsoles
    var outputconsole : MessageConsole = null
    if (existing.length > 0) {
      Console.println("have existing console(s) : " + existing.length)
      outputconsole = existing(0).asInstanceOf[MessageConsole]
    } else {
      Console.println("needed to create new console")
      val mycon = new MessageConsole("Coq", null)
      val cons = new Array[IConsole](1)
      cons(0) = mycon
      conman.addConsoles(cons)
      outputconsole = mycon
    }
    out = outputconsole.newMessageStream
  }

//   display console in workbench!
//   IWorkbenchPage page = ...; obtain the active page
//   String id = IConsoleConstants.ID_CONSOLE_VIEW;
//   IConsoleView view = (IConsoleView) page.showView(id);
//   view.display(myConsole);
}

