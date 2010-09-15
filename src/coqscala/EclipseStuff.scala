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
        val content = doc.get.drop(DocumentState.position)
        if (content.length > 0) {
          val eoc = findEnd(content)

          DocumentState.sendlen = eoc
          DocumentState.sourceview = texteditor.getSource //should only be called once, somehow!
          Console.println("command is (" + eoc + "): " + content.take(eoc))
          CoqTop.writeToCoq(content.take(eoc))
        } else { Console.println("EOF") }
      } else {
        Console.println("not a CoqEditor!")
      }
    } else
    	Console.println("not a ITextEditor!")
  }

  def findEnd (content : String) : Int = {
    var cont = true
    val comment = content.indexOf("(*")
    var endofcommand = 0
    if (comment < content.indexOf("."))
      endofcommand = content.indexOf("*)", comment + 2)
    while (cont) {
      val newend = content.indexOf(".", endofcommand + 1)
      if (newend == -1) cont = false
      else endofcommand = newend
      if (content(endofcommand - 1) != '.' && (content.startsWith(" ", endofcommand + 1) || content.startsWith("\n", endofcommand + 1)))
        cont = false
    }
    endofcommand + 2 //". "
  }


  override def selectionChanged (action : IAction, selection : ISelection) : Unit = { }

  override def dispose () : Unit = { }	
}

object CoqStepAction extends CoqStepAction { }

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

import org.eclipse.ui.part.ViewPart

class GoalViewer extends ViewPart {
  import org.eclipse.swt.widgets.{Composite,Label,Text}
  import org.eclipse.swt.SWT
  import org.eclipse.swt.layout.{GridData,GridLayout}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display


  var hypos : Text = null
  var goal : Label = null
  var othersubs : Text = null
  var comp : Composite = null

  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new GridLayout(1, true))
    hypos = new Text(comp, SWT.READ_ONLY | SWT.MULTI)
    hypos.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //hypos.setText("foo\nbar")
    new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL).setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    goal = new Label(comp, SWT.READ_ONLY)
    goal.setBackground(new Color(Display.getDefault, new RGB(255, 255, 255)))
    goal.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //goal.setText("baz")
    val other = new Label(comp, SWT.READ_ONLY)
    other.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    other.setText("other subgoals")
    othersubs = new Text(comp, SWT.READ_ONLY | SWT.MULTI)
    othersubs.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //othersubs.setText("buz\nfoobar")
    CoqOutputDispatcher.goalviewer = this
    PrintActor.register(CoqOutputDispatcher)
  }

  def setFocus() : Unit = {
  //  viewer.getControl.setFocus
  }
}

object GoalViewer extends GoalViewer { }

object CoqOutputDispatcher extends CoqCallback {
  import org.eclipse.swt.widgets.Display

  var goalviewer : GoalViewer = null
	
  override def dispatch (x : CoqResponse) : Unit = {
    val (ht, gt, ot) = x match {
      case CoqGoal(n, goals) => {
          val (hy, res) = goals.splitAt(goals.findIndexOf(_.contains("======")))
          val ht = if (hy.length > 0) hy.reduceLeft((x, y) => x + "\n" + y) else ""
          val subd = res.findIndexOf(_.contains("subgoal "))
          val (g, r) = if (subd > 0) res.splitAt(subd) else (res, List[String]())
          val gt = g.drop(1).reduceLeft((x, y) => x + " " + y)
          val ot = if (r.length > 0) {
            val r2 = r.map(x => { if (x.contains("subgoal ")) x.drop(1) else x })
            r2.reduceLeft((x, y) => x + "\n" + y)
          } else ""
          (ht, gt, ot)
        }
      case CoqProofCompleted() => ("Proof completed", "", "")
      case x => EclipseConsole.out.println("received: " + x); ("", "", "")
    }
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = {
          goalviewer.hypos.setText(ht)
          goalviewer.goal.setText(" " + gt)
          goalviewer.othersubs.setText(ot)
          goalviewer.comp.layout
        }
      })
  }
}
