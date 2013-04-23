package dk.itu.sdg.kopitiam

import org.eclipse.swt.widgets.{Composite, Control, Text}

trait GoalPresenter {
  def init(parent : Composite)
  def render(goals : Option[CoqTypes.goals])
  def dispose
}

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout

class DefaultGoalPresenter extends GoalPresenter {
  import org.eclipse.swt.widgets.{TabFolder, TabItem}
  
  private var goals : TabFolder = null
  override def init(parent : Composite) = {
    goals = new TabFolder(parent, SWT.NONE)
    goals
  }
  
  override def dispose = goals.dispose
  
  private def subgoals = goals.getItems()
  
  override def render(coqGoals : Option[CoqTypes.goals]) = {
    val goalData = coqGoals match {
      case None => List.empty
      case Some(x) => x.fg_goals
    }
    if (subgoals.length < goalData.length) {
      while (subgoals.length != goalData.length) {
        val ti = new TabItem(goals, SWT.NONE)
        val area = new Composite(goals, SWT.NONE)
        area.setLayout(new FillLayout(SWT.VERTICAL))
        ti.setControl(area)
        
        // val sash = new Sash(area, SWT.HORIZONTAL)
        new Text(area,
            SWT.BORDER | SWT.READ_ONLY | SWT.MULTI |
            SWT.H_SCROLL | SWT.V_SCROLL)
        new Text(area,
            SWT.BORDER | SWT.READ_ONLY | SWT.MULTI |
            SWT.H_SCROLL | SWT.V_SCROLL)
        ti.setText(subgoals.length.toString)
      }
    } else {
      while (subgoals.length != goalData.length)
        subgoals.last.dispose()
    }
    goals.pack
    goalData.zip(subgoals).foreach(_ match {
      case (goal, control) =>
        val comp = control.getControl().asInstanceOf[Composite]
        comp.getChildren()(0).asInstanceOf[Text].setText(goal.goal_hyp.mkString("\n"))
        comp.getChildren()(1).asInstanceOf[Text].setText(goal.goal_ccl)
    })
  }
}

import org.eclipse.ui.part.ViewPart
import org.eclipse.ui.{IPropertyListener, IPartListener2}

class GoalViewer extends ViewPart with IPropertyListener with IPartListener2 {
  import org.eclipse.swt.layout.{FormData,FormLayout,FormAttachment}
  import org.eclipse.swt.graphics.{Color,RGB,Rectangle}
  import org.eclipse.swt.widgets.{Display,Sash,Listener,Event}
//  import org.eclipse.swt.custom.{CTabFolder,CTabItem}
  
  override def propertyChanged (source : Object, propID : Int) = {
    if (source.isInstanceOf[CoqTopContainer] &&
        propID == CoqTopContainer.PROPERTY_GOALS)
      writeGoal(source.asInstanceOf[CoqTopContainer].goals)
  }

  import org.eclipse.ui.IViewSite
  override def init (site : IViewSite) = {
    super.init(site)
    site.getWorkbenchWindow().getPartService().addPartListener(this)
  }
  
  override def dispose = {
    presenter.dispose
    getSite.getWorkbenchWindow().getPartService().removePartListener(this)
    super.dispose
  }
  
  import org.eclipse.ui.IEditorPart
  private var activeContainer : Option[CoqTopContainer] = None
  private def setActiveContainer (e : IEditorPart) = {
    println("" + this + ".setActiveEditor(" + e + ")")
    activeContainer match {
      case Some(ed) => ed.removeListener(this)
      case None =>
    }
    activeContainer = Option(e).flatMap(a => {
      val klass = classOf[CoqTopContainer]
      val adapter = a.getAdapter(klass)
      if (klass.isInstance(adapter)) {
        Some(klass.cast(adapter))
      } else None
    })
    activeContainer match {
      case Some(c) =>
        c.addListener(this)
        writeGoal(c.goals)
      case None =>
        writeGoal(None)
    }
  }
  
  import org.eclipse.ui.IWorkbenchPartReference
  override def partOpened (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (p == this)
      setActiveContainer(
          getSite.getWorkbenchWindow().getActivePage().getActiveEditor())
  }
  
  override def partClosed (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (this == p || Some(p) == activeContainer)
      setActiveContainer(null)
  }
  
  override def partActivated (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (p.isInstanceOf[IEditorPart] && Some(p) != activeContainer)
      setActiveContainer(p.asInstanceOf[IEditorPart])
  }

  override def partDeactivated (ref : IWorkbenchPartReference) = ()

  override def partBroughtToTop (part : IWorkbenchPartReference) : Unit = { }
  override def partHidden (part : IWorkbenchPartReference) : Unit = { }
  override def partInputChanged (part : IWorkbenchPartReference) : Unit = { }
  override def partVisible (part : IWorkbenchPartReference) : Unit = { }

  private var presenter : GoalPresenter = new DefaultGoalPresenter
  
  var comp : Composite = null

  import org.eclipse.swt.layout.FillLayout
  
  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new FillLayout())

    presenter.init(comp)
  }

  /*class SashListener(sash : Sash, comp : Composite, limit : Int)
      extends Listener {
    override def handleEvent (e : Event) = {
      val sashRect : Rectangle = sash.getBounds()
      val shellRect : Rectangle = comp.getClientArea()
      val top = shellRect.height - sashRect.height - limit
      e.y = scala.math.max(scala.math.min(e.y, top), limit)
      if (e.y != sashRect.y)  {
        sashData.top = new FormAttachment (0, e.y)
        comp.layout()
      }
    }
  }*/
  
  private def writeGoal (coqGoals : Option[CoqTypes.goals]) : Unit = {
    if (!comp.isDisposed)
      presenter.render(coqGoals)
    comp.layout
  }

  def setFocus() : Unit = {
  //  viewer.getControl.setFocus
  }
}