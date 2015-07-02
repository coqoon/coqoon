/* Responses.scala
 * Utility methods for extracting PIDE markup, results, goals and errors
 * Copyright © 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.pide

object Responses {
  import isabelle._
  import dk.itu.coqoon.core.coqtop.CoqTypes
  def extractMarkup(snapshot : Document.Snapshot,
      command : isabelle.Command) : Seq[XML.Elem] = {
    val markupTree =
      snapshot.state.command_markup(snapshot.version, command,
          Command.Markup_Index.markup, command.range, Markup.Elements.full)
    (for ((range, entry) <- markupTree.branches;
          markup <- entry.markup)
       yield markup).toSeq
  }

  /* The left-hand side of this Either is an error message, and the right is
   * a normal status message. */
  def extractQueryResult(snapshot : Document.Snapshot,
      command : isabelle.Command, queryID_ : Long) : Option[Either[String, String]] = {
    val queryID = queryID_.toString

    import isabelle.XML.Elem
    val results = (extractResults(snapshot, command) collect {
      case (_, e @ Elem(Markup(Markup.RESULT, properties), _))
          if properties.contains(Markup.INSTANCE -> queryID) =>
        e
    })

    val finished = results exists {
      case XML.Elem(_, List(XML.Elem(Markup(Markup.STATUS, _),
                            List(XML.Elem(Markup(Markup.FINISHED, _), _))))) =>
        true
      case _ =>
        false
    }

    if (finished) {
      val r =
        results collectFirst {
          case XML.Elem(_, List(XML.Elem(Markup(Markup.ERROR, _),
              List(t : XML.Text)))) =>
            Left(t.content.trim)
          case XML.Elem(_, List(XML.Elem(Markup(Markup.WRITELN, _),
              List(t : XML.Text)))) =>
            Right(t.content.trim)
        }
      r.orElse(Some(Left("Query finished without producing output")))
    } else None
  }

  def extractResults(snapshot : Document.Snapshot,
      command : isabelle.Command) : Seq[Command.Results.Entry] = {
    val results =
      snapshot.state.command_results(snapshot.version, command)
    results.iterator.toSeq
  }

  import isabelle.XML.{Elem, Text, Tree}
  /* For the time being, we convert exciting new PIDE data into boring old
   * -ideslave-8.4 data, to make it easier to support both at once. */
  private object GoalAssist {
    import dk.itu.coqoon.core.coqtop.CoqTypes
    def extractGoalList(e : Tree) : List[CoqTypes.goal] = e match {
      case e : Elem =>
        e.body.flatMap(extractGoal)
      case _ => List()
    }
    def extractGoal(e : Tree) : Option[CoqTypes.goal] = e match {
      case e : Elem if e.name == "goal" =>
        val propertyMap = e.markup.properties.toMap
        Some(CoqTypes.goal(
            propertyMap.get("id").getOrElse("(unknown)"),
            extractHypotheses(e.body(0)),
            e.body(1).asInstanceOf[Elem].body(0).asInstanceOf[Text].content))
      case _ => None
    }
    import dk.itu.coqoon.core.utilities.TryCast
    def extractHypotheses(e : Tree) : List[String] = e match {
      case e : Elem if e.name == "hypotheses" =>
        for (hypothesis <- e.body.flatMap(TryCast[Elem]);
             text <- TryCast[Text](hypothesis.body(0)))
          yield text.content
      case _ => List()
    }
  }
  def extractGoals(e : Tree) = e match {
    case e : Elem if e.name == "goals" =>
      Some(CoqTypes.goals(GoalAssist.extractGoalList(e.body(0)), List()))
    case _ => None
  }

  def extractError(e : Tree) = e match {
    case f : Elem if f.name == "error_message" =>
      val propertyMap = f.markup.properties.toMap
      val (errStart, errEnd) =
        (propertyMap.get("offset").map(Integer.parseInt(_, 10)),
         propertyMap.get("end_offset").map(Integer.parseInt(_, 10)))
      val msg = f.body(0).asInstanceOf[Text].content
      propertyMap.get("id").map(Integer.parseInt(_, 10)).map(
          id => (id, msg, errStart, errEnd))
    case _ => None
  }
}