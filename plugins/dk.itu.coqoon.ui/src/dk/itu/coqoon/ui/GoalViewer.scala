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

  /* The Composites passed to this method have FillLayouts */
  protected def makeTabRegions(top : Composite, bottom : Composite) : Unit

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

    makeTabRegions(top, bottom)

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

class RawGoalPresenter extends SashGoalPresenter {
  override protected def makeTabRegions(
      top : Composite, bottom : Composite) = {
    new Text(top,
        SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)
    new Text(bottom,
        SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)
  }

  override protected def updateTab(
      goal : CoqTypes.goal, top : Composite, bottom : Composite) = {
    top.getChildren()(0).asInstanceOf[Text].setText(
        goal.goal_hyp.mkString("\n"))
    bottom.getChildren()(0).asInstanceOf[Text].setText(goal.goal_ccl)
  }
}

import dk.itu.coqoon.core.utilities.Substring

class RichGoalPresenter extends SashGoalPresenter {
  import org.eclipse.swt.custom.{StyleRange, StyledText}

  private var selection : Seq[String] = Nil

  private def getCaretIdentifier(topT : StyledText) : Option[String] = {
    val caret = topT.getCaretOffset
    val idents = TryCast[Seq[(String, Int, Int)]](
        topT.getData("cqidents")).getOrElse(Seq())
    for ((name, start, end) <- idents if caret >= start && caret < end)
      return Some(name)
    None
  }

  private var suppressUpdate = false

  override protected def makeTabRegions(
      top : Composite, bottom : Composite) = {
    import org.eclipse.swt.custom.{CaretEvent, CaretListener}
    val topT = new StyledText(top,
        SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)
    val bottomT = new StyledText(bottom,
        SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)
    topT.addCaretListener(new CaretListener {
      override def caretMoved(ev : CaretEvent) = if (!suppressUpdate) {
        val idents = TryCast[Seq[(String, Int, Int)]](
            topT.getData("cqidents")).getOrElse(Seq())
        val names = idents.map(_._1)

        for (ctrl <- Seq(topT, bottomT)) {
          var parts : Seq[Substring] = Seq()
          RichGoalPresenter.handleTokens(ctrl.getText, part =>
              if (names.contains(part.toString)) parts :+= part)
          ctrl.setStyleRanges(
              toStyleRanges(parts, getCaretIdentifier(topT)).toArray)
        }
      }
    })
  }

  import dk.itu.coqoon.ui.utilities.UIUtils
  private final val RED = UIUtils.Color(255, 0, 0)

  private def toStyleRanges(
      contextIdentifiers : Seq[Substring], focus : Option[String]) =
    contextIdentifiers.map(i => new StyleRange {
      this.start = i.start
      this.length = i.length
      this.fontStyle = SWT.BOLD
      if (focus.toSeq.contains(i.toString))
        this.foreground = RED
    })

  override protected def updateTab(
      goal : CoqTypes.goal, top : Composite, bottom : Composite) = {
    val topT = top.getChildren()(0).asInstanceOf[StyledText]
    val bottomT = bottom.getChildren()(0).asInstanceOf[StyledText]

    var offset : Int = 0
                   /* name, start, end */
    var idents : Seq[(String, Int, Int)] = Seq()
    goal.goal_hyp.foreach(hypothesis => {
      val end = offset + hypothesis.length + 1
      idents :+= (hypothesis.split(":", 2).head.trim, offset, end)
      offset = end
    })
    topT.setData("cqidents", idents)

    val names = idents.map(_._1)

    try {
      suppressUpdate = true
      Seq(topT, bottomT).foreach(_.setStyleRanges(Array()))
      topT.setText(goal.goal_hyp.mkString("\n"))
      bottomT.setText(goal.goal_ccl)
    } finally suppressUpdate = false

    for (ctrl <- Seq(topT, bottomT)) {
      var parts : Seq[Substring] = Seq()
      RichGoalPresenter.handleTokens(ctrl.getText, part =>
          if (names.contains(part.toString)) parts :+= part)
      ctrl.setStyleRanges(
          toStyleRanges(parts, getCaretIdentifier(topT)).toArray)
    }
  }
}
object RichGoalPresenter {
  private def handleTokens(
      input_ : String, callback : Substring => Unit) = {
    val input = input_ :+ '\u0000'
    var contextIdentifiers : Seq[Substring] = Seq()
    var (offset, detectionStart) = (0, Option.empty[Int])
    while (offset < input.length) {
      val i = input(offset)
      if (detectionStart == None) {
        if (CoqWordDetector.isWordStart(i))
          detectionStart = Some(offset)
      } else if (!CoqWordDetector.isWordPart(i)) {
        while (offset > 0 && !CoqWordDetector.isWordEnd(input(offset - 1)))
          offset -= 1
        callback(Substring(input, detectionStart.get, offset))
        detectionStart = None
      }
      offset += 1
    }
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
    extends AttentiveViewPart[CoqGoalsContainer] with IPropertyListener {
  import org.eclipse.swt.layout.{FormData,FormLayout,FormAttachment}

  override def propertyChanged (source : Object, propID : Int) = {
    if (source.isInstanceOf[CoqGoalsContainer] &&
        propID == CoqGoalsContainer.PROPERTY_GOALS)
      writeGoal(source.asInstanceOf[CoqGoalsContainer].goals)
  }

  override def dispose = {
    setPresenter(null)
    super.dispose
  }

  override protected def detach(part : CoqGoalsContainer) = {
    part.removeListener(this)
    writeGoal(None)
  }

  override protected def attach(part : CoqGoalsContainer) = {
    part.addListener(this)
    writeGoal(part.goals)
  }

  private var presenter : GoalPresenter = null

  var comp : Composite = null

  import org.eclipse.swt.layout.FillLayout

  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new FillLayout())
    setPresenter(new RichGoalPresenter)

    import org.eclipse.jface.action.{Action, IAction}
    val mm = getViewSite.getActionBars.getMenuManager
    mm.add(new Action("No presenter") {
      override def run =
        if (presenter != null)
          setPresenter(null)
    })
    mm.add(new Action("Rich presenter") {
      override def run =
        if (presenter == null || !presenter.isInstanceOf[RichGoalPresenter])
          setPresenter(new RichGoalPresenter)
    })
    mm.add(new Action("Basic presenter") {
      override def run =
        if (presenter == null || !presenter.isInstanceOf[RawGoalPresenter])
          setPresenter(new RawGoalPresenter)
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