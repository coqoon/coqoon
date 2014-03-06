package dk.itu.coqoon.ui

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
  import org.eclipse.swt.custom.{StyleRange, StyledText}
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
    new StyledText(parent,
      SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)

  import dk.itu.coqoon.core.utilities.Substring
  private def highlightContextIdentifiers(
      text : StyledText, contextIdentifiers : Seq[Substring]) = {
    val q =
      for (i <- contextIdentifiers)
        yield new StyleRange {
          this.start = i.start
          this.length = i.length
          this.fontStyle = SWT.BOLD
        }
    text.setStyleRanges(q.toArray)
  }

  override protected def updateTab(
      goal : CoqTypes.goal, top : Composite, bottom : Composite) = {
    val table = top.getChildren()(0).asInstanceOf[Table]
    if (table.getItems.length < goal.goal_hyp.length) {
      while (table.getItems.length < goal.goal_hyp.length)
        new TableItem(table, SWT.NONE)
    } else table.getItems.drop(goal.goal_hyp.length).map(_.dispose)
    table.getColumns.map(_.pack)

    var names : Seq[String] = Seq()
    table.getItems.zip(goal.goal_hyp).foreach(_ match {
      case (entry, hypothesis) =>
        val parts = hypothesis.split(":", 2).map(_.trim)
        names :+= parts(0)
        entry.setText(parts)
    })

    val text = bottom.getChildren()(0).asInstanceOf[StyledText]
    text.setStyleRanges(Array())

    var contextIdentifiers : Seq[Substring] = Seq()
    var (offset, detectionStart) = (0, Option.empty[Int])
    for (i <- goal.goal_ccl :+ '\0') {
      if (detectionStart == None) {
        if (CoqWordDetector.isWordStart(i))
          detectionStart = Some(offset)
      } else if (!CoqWordDetector.isWordPart(i)) {
        val word = Substring(goal.goal_ccl, detectionStart.get, offset)
        if (names.contains(word.toString))
          contextIdentifiers :+= word
        detectionStart = None
      }
      offset += 1
    }

    text.setText(goal.goal_ccl)
    highlightContextIdentifiers(text, contextIdentifiers)
  }
}

import org.eclipse.ui.part.ViewPart
import org.eclipse.ui.{IPropertyListener, IPartListener2}

abstract class AttentiveViewPart[A](
    implicit arg0 : Manifest[A]) extends ViewPart with IPartListener2 {
  import org.eclipse.ui.IViewSite
  override def init (site : IViewSite) = {
    super.init(site)
    site.getWorkbenchWindow.getPartService.addPartListener(this)
  }

  override def dispose = {
    getSite.getWorkbenchWindow.getPartService.removePartListener(this)
    super.dispose
  }

  import org.eclipse.ui.{IEditorPart, IWorkbenchPart}

  protected def attach(part : A)
  protected def detach(part : A)

  private var activePart : Option[A] = None
  private def setActivePart(e : IWorkbenchPart) = {
    activePart.foreach(detach)
    activePart = TryAdapt[A](e)
    activePart.foreach(attach)
  }

  protected def getActivePart() : Option[A] = activePart

  import org.eclipse.ui.IWorkbenchPartReference
  override def partOpened(ref : IWorkbenchPartReference) = {
    val part = ref.getPart(false)
    if (part == this)
      setActivePart(getSite.getWorkbenchWindow.getActivePage.getActiveEditor)
  }

  override def partClosed(ref : IWorkbenchPartReference) = {
    val part = ref.getPart(false)
    if (this == part || TryAdapt[A](part) == activePart)
      setActivePart(null)
  }

  override def partActivated (ref : IWorkbenchPartReference) = {
    val p = ref.getPart(false)
    if (p.isInstanceOf[IEditorPart] && TryAdapt[A](p) != activePart)
      setActivePart(p)
  }

  override def partHidden(r : IWorkbenchPartReference) = ()
  override def partVisible(r : IWorkbenchPartReference) = ()
  override def partDeactivated(r : IWorkbenchPartReference) = ()
  override def partBroughtToTop(r : IWorkbenchPartReference) = ()
  override def partInputChanged(r : IWorkbenchPartReference) = ()
}

class GoalViewer
    extends AttentiveViewPart[CoqTopContainer] with IPropertyListener {
  import org.eclipse.swt.layout.{FormData,FormLayout,FormAttachment}

  override def propertyChanged (source : Object, propID : Int) = {
    if (source.isInstanceOf[CoqTopContainer] &&
        propID == CoqTopContainer.PROPERTY_GOALS)
      writeGoal(source.asInstanceOf[CoqTopContainer].goals)
  }

  override def dispose = {
    setPresenter(null)
    super.dispose
  }

  override protected def detach(part : CoqTopContainer) = {
    part.removeListener(this)
    writeGoal(None)
  }

  override protected def attach(part : CoqTopContainer) = {
    part.addListener(this)
    writeGoal(part.goals)
  }

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
      writeGoal(getActivePart.flatMap(_.goals))
    }
  }

  private def writeGoal (coqGoals : Option[CoqTypes.goals]) : Unit = {
    if (!comp.isDisposed && presenter != null)
      presenter.render(coqGoals)
    comp.layout
  }

  def setFocus() : Unit = comp.setFocus
}