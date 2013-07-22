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
        
        new Text(area,
            SWT.BORDER | SWT.READ_ONLY | SWT.MULTI |
            SWT.H_SCROLL | SWT.V_SCROLL)
        new Text(area,
            SWT.BORDER | SWT.READ_ONLY | SWT.MULTI |
            SWT.H_SCROLL | SWT.V_SCROLL)
        ti.setText(subgoals.length.toString)
      }
    } else subgoals.drop(goalData.length).map(_.dispose)
    goals.pack
    goalData.zip(subgoals).foreach(_ match {
      case (goal, control) =>
        val comp = control.getControl().asInstanceOf[Composite]
        comp.getChildren()(0).asInstanceOf[Text].setText(goal.goal_hyp.mkString("\n"))
        comp.getChildren()(1).asInstanceOf[Text].setText(goal.goal_ccl)
    })
  }
}

class TabularGoalPresenter extends GoalPresenter {
  import org.eclipse.swt.widgets.{TabFolder, TabItem}
  import org.eclipse.swt.widgets.{Table, TableItem, TableColumn}
  
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
    if (subgoals.length <= goalData.length) {
      while (subgoals.length != goalData.length) {
        val ti = new TabItem(goals, SWT.NONE)
        val area = new Composite(goals, SWT.NONE)
        area.setLayout(new FillLayout(SWT.VERTICAL))
        ti.setControl(area)
        
        val ta = new Table(area, SWT.BORDER | SWT.MULTI)
        new TableColumn(ta, SWT.LEFT).setText("Name")
        new TableColumn(ta, SWT.LEFT).setText("Value")
        ta.setColumnOrder(Array(0, 1))
        ta.setLinesVisible(true)
        ta.setHeaderVisible(true)
        new Text(area,
            SWT.BORDER | SWT.READ_ONLY | SWT.MULTI |
            SWT.H_SCROLL | SWT.V_SCROLL)
        ti.setText(subgoals.length.toString)
      }
    } else subgoals.drop(goalData.length).map(_.dispose)
    goals.pack
    goalData.zip(subgoals).foreach(_ match {
      case (goal, control) =>
        val comp = control.getControl().asInstanceOf[Composite]
        
        val table = comp.getChildren()(0).asInstanceOf[Table]
        if (table.getItems.length < goal.goal_hyp.length) {
          while (table.getItems.length < goal.goal_hyp.length)
            new TableItem(table, SWT.NONE)
        } else table.getItems.drop(goal.goal_hyp.length).map(_.dispose)
        table.getColumns.map(_.pack)
        table.getItems.zip(goal.goal_hyp).foreach(_ match {
          case (entry, hypothesis) =>
            entry.setText(hypothesis.split(":", 2).map(_.trim))
        })
        
        comp.getChildren()(1).asInstanceOf[Text].setText(goal.goal_ccl)
    })
  }
}

import org.eclipse.ui.part.ViewPart
import org.eclipse.ui.{IPropertyListener, IPartListener2}

class GoalViewer extends ViewPart with IPropertyListener with IPartListener2 {
  import org.eclipse.swt.layout.{FormData,FormLayout,FormAttachment}
  
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
    setPresenter(null)
    getSite.getWorkbenchWindow().getPartService().removePartListener(this)
    super.dispose
  }
  
  import org.eclipse.ui.{IEditorPart, IWorkbenchPart}
  private var activeContainer : Option[CoqTopContainer] = None
  private def setActiveContainer(e : IWorkbenchPart) = {
    activeContainer match {
      case Some(ed) => ed.removeListener(this)
      case None =>
    }
    activeContainer = TryAdapt[CoqTopContainer](e)
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
    if (this == p || TryAdapt[CoqTopContainer](p) == activeContainer)
      setActiveContainer(null)
  }
  
  override def partActivated (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (p.isInstanceOf[IEditorPart] &&
        TryAdapt[CoqTopContainer](p) != activeContainer)
      setActiveContainer(p)
  }

  override def partDeactivated (ref : IWorkbenchPartReference) = ()

  override def partBroughtToTop (part : IWorkbenchPartReference) : Unit = { }
  override def partHidden (part : IWorkbenchPartReference) : Unit = { }
  override def partInputChanged (part : IWorkbenchPartReference) : Unit = { }
  override def partVisible (part : IWorkbenchPartReference) : Unit = { }

  private var presenter : GoalPresenter = null
  
  var comp : Composite = null

  import org.eclipse.swt.layout.FillLayout
  
  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new FillLayout())
    setPresenter(new DefaultGoalPresenter)
    
    import org.eclipse.jface.action.{Action, IAction}
    val mm = getViewSite.getActionBars.getMenuManager
    mm.add(new Action("No presenter") {
      override def run =
        if (presenter != null)
          setPresenter(null)
    })
    mm.add(new Action("Default presenter") {
      override def run =
        if (presenter == null || !presenter.isInstanceOf[DefaultGoalPresenter])
          setPresenter(new DefaultGoalPresenter)
    })
    mm.add(new Action("Tabular presenter") {
      override def run =
        if (presenter == null || !presenter.isInstanceOf[TabularGoalPresenter])
          setPresenter(new TabularGoalPresenter)
    })
  }

  private def setPresenter(gp : GoalPresenter) = {
    if (presenter != null)
      presenter.dispose
    presenter = gp
    if (presenter != null) {
      presenter.init(comp)
      writeGoal(activeContainer.flatMap(_.goals))
    }
  }
  
  private def writeGoal (coqGoals : Option[CoqTypes.goals]) : Unit = {
    if (!comp.isDisposed && presenter != null)
      presenter.render(coqGoals)
    comp.layout
  }

  def setFocus() : Unit = comp.setFocus
}