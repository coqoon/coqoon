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
      command : isabelle.Command) : Seq[(Text.Range, XML.Elem)] = {
    def _extract(t : Markup_Tree) : Seq[(Text.Range, XML.Elem)] = {
      val m =
        for ((range, entry) <- t.branches.toList;
             markup <- entry.markup)
        yield (range, markup)
      val c =
        t.branches.flatMap(b => _extract(b._2.subtree)).toList
      m.toList ++ c
    }
    val markupTree =
      snapshot.state.command_markup(snapshot.version, command,
          Command.Markup_Index.markup, command.range, Markup.Elements.full)
    _extract(markupTree)
  }

  /* The left-hand sides of the returned Eithers are error messages, and the
   * right sides are normal status messages. */
  def extractQueryResult(snapshot : Document.Snapshot,
      command : isabelle.Command, queryID_ : Long) : Seq[Either[String, String]] = {
    val queryID = queryID_.toString

    import isabelle.XML.Elem
    val results = extractResults(snapshot, command) collect {
      case (_, e @ Elem(Markup(Markup.RESULT, properties), _))
          if properties.contains(Markup.INSTANCE -> queryID) =>
        e
    }

    val finished = results exists {
      case XML.Elem(_, List(XML.Elem(Markup(Markup.STATUS, _),
                            List(XML.Elem(Markup(Markup.FINISHED, _), _))))) =>
        true
      case _ =>
        false
    }

    if (finished) {
      val r =
        results collect {
          case XML.Elem(_, List(XML.Elem(Markup(Markup.ERROR, _),
              List(t : XML.Text)))) =>
            Left(t.content.trim)
          case XML.Elem(_, List(XML.Elem(Markup(Markup.WRITELN, _),
              List(t : XML.Text)))) =>
            Right(t.content.trim)
        }
      if (!r.isEmpty) {
        if (r.forall(_.isRight)) {
          r
        } else {
          /* If there was *any* error output, then suppress the normal output;
           * Coq has a tendency to copy errors as normal status messages */
          r.filter(_.isLeft)
        }
      } else Seq(Left("Query finished without producing output"))
    } else Seq()
  }

  def extractResults(snapshot : Document.Snapshot,
      command : isabelle.Command) : Seq[Command.Results.Entry] = {
    val results =
      snapshot.state.command_results(snapshot.version, command)
    results.iterator.toSeq
  }

  import isabelle.XML.{Elem, Text, Tree}
  def extractWritelnMessage(e : isabelle.XML.Tree) = e match {
    case Elem(Markup(Markup.WRITELN_MESSAGE, properties),
              List(t : Text)) if !properties.contains("source" -> "goal") =>
      Some(t.content)
    case _ => None
  }

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
    case Elem(Markup("goals", _),
              List(focus, bg, shelf, given_up)) =>
      import GoalAssist.{extractGoalList => eGL}
      val (focused_goals, shelved_goals, given_up_goals) =
        (eGL(focus), eGL(shelf), eGL(given_up))
      val bg_goals =
        bg match {
          case Elem(Markup("bg_goals", _), bg_goals) =>
            /* This is a list of pairs of lists of goals */
            for (Elem(Markup("bg_goal", _), List(l, r)) <- bg_goals)
              yield (eGL(l), eGL(r))
          case _ =>
            List()
        }
      Some(CoqTypes.goals(
          focused_goals, bg_goals, shelved_goals, given_up_goals))
    case _ => None
  }

  def extractError(e : Tree) = e match {
    case Elem(
        Markup(Markup.ERROR_MESSAGE, properties),
        List(message : Text)) =>
      val propertyMap = properties.toMap
      val (errStart, errEnd) =
        (propertyMap.get("offset").map(Integer.parseInt(_, 10)),
         propertyMap.get("end_offset").map(Integer.parseInt(_, 10)))
      propertyMap.get("id").map(Integer.parseInt(_, 10)).map(
          id => (id, message.content, errStart, errEnd))
    case _ => None
  }

  def extractWarning(e : Tree) = e match {
    case Elem(
        Markup(Markup.WARNING_MESSAGE, properties),
        List(message : Text)) =>
      val propertyMap = properties.toMap
      val (errStart, errEnd) =
        (propertyMap.get("offset").map(Integer.parseInt(_, 10)),
         propertyMap.get("end_offset").map(Integer.parseInt(_, 10)))
      propertyMap.get("id").map(Integer.parseInt(_, 10)).map(
          id => (id, message.content, errStart, errEnd))
    case _ => None
  }

  /* The left tuple is (path to file, (start file offset, end file offset)),
   * and the right, used for things defined in this PIDE session, is (defining
   * execution ID, (start command offset, end command offset)). All offsets
   * are zero-based (or have been corrected to be zero-based). */
  type Entity = Either[
    (String, (Int, Int)),
    (isabelle.Document_ID.Generic, (Int, Int))]
  def extractEntity(e : Tree) : Option[Entity] = e match {
    case Elem(Markup(Markup.ENTITY, properties), _) =>
      val propertyMap = properties.toMap
      Seq("def_file", "def_offset", "def_end_offset", "def_id").
          map(propertyMap.get) match {
        case Seq(
            Some(def_file), Some(def_offset_), Some(def_end_offset_), _) =>
          val def_offset = Integer.parseInt(def_offset_, 10)
          val def_end_offset = Integer.parseInt(def_end_offset_, 10)
          Some(Left(def_file, (def_offset - 1, def_end_offset)))
        case Seq(
            None, Some(def_offset_), Some(def_end_offset_), Some(def_id_)) =>
          val def_id = Integer.parseInt(def_id_, 10)
          val def_offset = Integer.parseInt(def_offset_, 10)
          val def_end_offset = Integer.parseInt(def_end_offset_, 10)
          Some(Right(def_id, (def_offset - 1, def_end_offset - 1)))
        case x =>
          None
      }
    case _ => None
  }
}