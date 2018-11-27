package dk.itu.coqoon.core.coqtop.coqidetop

trait CoqIdeTop_v20170413 {
  import Interface._
  def about() : value[coq_info]
  def add(stateId : Integer, command : CharSequence, v : Interface.verbose) :
      value[(state_id, (Either[Unit, state_id], String))]
  // def annotate(annotation : String) : value[scala.xml.Elem]
  def editAt(stateId : Integer) : value[Either[Unit, (state_id, (state_id, state_id))]]
  def evars() : value[Option[List[String]]]
  def getOptions() : value[List[(List[String], option_state)]]
  def goal() : value[Option[goals]]
  def hints() : value[Option[(List[hint], hint)]]
  def init(scriptPath : Option[String]) : value[state_id]
  def mkCases(s : String) : value[List[List[String]]]
  // def printAst(stateId : Integer) : value[scala.xml.Elem]
  def query(routeId : Integer, query : String,
      stateId : Integer) : value[String]
  /* def quit() = unwrapQuitResponse(send(wrapQuitCall)) */
  def search(constraints : Seq[(Interface.search_constraint, Boolean)]) :
      value[List[coq_object[String]]]
  def setOptions(options : Seq[(Seq[String], Interface.option_value)]) :
      value[Unit]
  def status(force : Boolean) : value[status]
  def stopWorker(worker : String) : value[Unit]

  protected var listeners : Set[CoqIdeTopFeedbackListener] = Set()
  def addListener(l : CoqIdeTopFeedbackListener) = (listeners += l)
  def removeListener(l : CoqIdeTopFeedbackListener) = (listeners -= l)
}
object CoqIdeTop_v20170413 {
  def apply() : CoqIdeTop_v20170413 = apply(Seq.empty)

  def apply(args : Seq[String]) : CoqIdeTop_v20170413 = new CoqIdeTopImpl(args)
}

trait CoqIdeTopFeedbackListener {
  def onFeedback(f : Feedback)
}