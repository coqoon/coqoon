package dk.itu.sdg.kopitiam

import dk.itu.coqoon.core.coqtop.CoqTypes
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}

import org.eclipse.swt.widgets.{Text, Control, Composite}

trait GoalPresenter {
  def init(parent : Composite)
  def render(goals : Option[CoqTypes.goals])
  def dispose
}

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout

import org.eclipse.swt.custom.{
  CTabItem => TabItemImpl, CTabFolder => TabFolderImpl}

abstract class TabbedGoalPresenter extends GoalPresenter {
  private var goals_ : TabFolderImpl = null
  protected def goals : TabFolderImpl = goals_
  protected def subgoals = goals.getItems()

  override def init(parent : Composite) = {
    goals_ = new TabFolderImpl(parent, SWT.BORDER)
  }

  protected def makeTab(parent : Composite) : Control
  protected def updateTab(goal : CoqTypes.goal, control : Control) : Unit

  override def render(coqGoals : Option[CoqTypes.goals]) = {
    val goalData = coqGoals match {
      case None => List.empty
      case Some(x) => x.fg_goals
    }
    if (subgoals.length < goalData.length) {
      while (subgoals.length != goalData.length) {
        val ti = new TabItemImpl(goals, SWT.NONE)
        ti.setControl(makeTab(goals))
        ti.setText(subgoals.length.toString)
      }
    } else subgoals.drop(goalData.length).map(_.dispose)
    goals.layout
    if (goals.getSelection == null)
      goals.getItems().headOption.map(goals.setSelection)
    goalData.zip(subgoals).foreach(a => updateTab(a._1, a._2.getControl))
  }

  override def dispose = goals_.dispose
}

abstract class SashGoalPresenter extends TabbedGoalPresenter {
  import org.eclipse.swt.layout.{FormData, FormLayout, FormAttachment}

  /* The Composite passed to these methods has a FormLayout */
  protected def makeTabTop(parent : Composite) : Unit
  protected def makeTabBottom(parent : Composite) : Unit

  final val sharedSashData = new FormData

  override def init(parent : Composite) = {
    super.init(parent)

    sharedSashData.left = new FormAttachment(0, 0)
    sharedSashData.right = new FormAttachment(100, 0)
    sharedSashData.top = new FormAttachment(50, 0)
  }

  override protected def makeTab(parent : Composite) = {
    import org.eclipse.swt.widgets.{Sash, Event, Listener}

    val area = new Composite(parent, SWT.NONE)
    area.setLayout(new FormLayout)

    val top = new Composite(area, SWT.NONE)
    val sash = new Sash(area, SWT.HORIZONTAL)
    val bottom = new Composite(area, SWT.NONE)

    top.setLayout(new FillLayout(SWT.VERTICAL))
    val topData = new FormData
    topData.left = new FormAttachment(0)
    topData.right = new FormAttachment(100)
    topData.top = new FormAttachment(0)
    topData.bottom = new FormAttachment(sash)
    top.setLayoutData(topData)
    makeTabTop(top)

    sash.addListener(SWT.Selection, new Listener {
      private final val LIMIT = 20 /* px */

      override def handleEvent(ev : Event) {
        val sashBounds = sash.getBounds
        val parentBounds = sash.getParent.getClientArea
        val top = parentBounds.height - sashBounds.height - LIMIT
        val ny = Math.max(Math.min(ev.y, top), LIMIT)
        if (ny != sashBounds.y) {
          sharedSashData.top = new FormAttachment(ny, parentBounds.height, 0)
          parent.layout(true, true) /* Redraw the other tabs, too */
        }
      }
    })
    sash.setLayoutData(sharedSashData)

    bottom.setLayout(new FillLayout(SWT.VERTICAL))
    val bottomData = new FormData
    bottomData.left = new FormAttachment(0)
    bottomData.right = new FormAttachment(100)
    bottomData.top = new FormAttachment(sash)
    bottomData.bottom = new FormAttachment(100)
    bottom.setLayoutData(bottomData)
    makeTabBottom(bottom)

    area
  }

  protected def updateTab(
      goal : CoqTypes.goal, top : Composite, bottom : Composite) : Unit
  override protected final def updateTab(
      goal : CoqTypes.goal, control : Control) =
    TryCast[Composite](control).map(_.getChildren()).getOrElse(
        Array.empty).flatMap(TryCast[Composite]) match {
      case Array(top, bottom) => updateTab(goal, top, bottom)
      case _ =>
    }
}

class DefaultGoalPresenter extends SashGoalPresenter {
  override protected def makeTabTop(parent : Composite) =
    new Text(parent,
        SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)

  override protected def makeTabBottom(parent : Composite) =
    new Text(parent,
        SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)

  override protected def updateTab(
      goal : CoqTypes.goal, top : Composite, bottom : Composite) = {
    top.getChildren()(0).asInstanceOf[Text].setText(
        goal.goal_hyp.mkString("\n"))
    bottom.getChildren()(0).asInstanceOf[Text].setText(goal.goal_ccl)
  }
}

class TabularGoalPresenter extends SashGoalPresenter {
  import org.eclipse.swt.widgets.{Table, TableItem, TableColumn}

  override protected def makeTabTop(parent : Composite) = {
    val ta = new Table(parent, SWT.BORDER | SWT.MULTI)
    new TableColumn(ta, SWT.LEFT).setText("Name")
    new TableColumn(ta, SWT.LEFT).setText("Value")
    ta.setColumnOrder(Array(0, 1))
    ta.setLinesVisible(true)
    ta.setHeaderVisible(true)
  }

  override protected def makeTabBottom(parent : Composite) =
    new Text(parent,
      SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)

  override protected def updateTab(
      goal : CoqTypes.goal, top : Composite, bottom : Composite) = {
    val table = top.getChildren()(0).asInstanceOf[Table]
    if (table.getItems.length < goal.goal_hyp.length) {
      while (table.getItems.length < goal.goal_hyp.length)
        new TableItem(table, SWT.NONE)
    } else table.getItems.drop(goal.goal_hyp.length).map(_.dispose)
    table.getColumns.map(_.pack)
    table.getItems.zip(goal.goal_hyp).foreach(_ match {
      case (entry, hypothesis) =>
        entry.setText(hypothesis.split(":", 2).map(_.trim))
    })

    bottom.getChildren()(0).asInstanceOf[Text].setText(goal.goal_ccl)
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