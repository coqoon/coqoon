package dk.itu.coqoon.core.coqtop.coqidetop

import scala.xml.{Elem, Node, Text, Attribute}

object Interface {
  /* This is just a Scala translation of ide/interface.mli */
  type raw = Boolean
  type verbose = Boolean

  case class goal(
    goal_id : String,
    goal_hyp : List[String],
    goal_ccl : String)

  case class evar(
    evar_info : String)

  case class status(
    status_path : List[String],
    status_proofname : Option[String],
    status_allproofs : List[String],
    status_proofnum : Int)

  case class goals(
    fg_goals : List[goal],
    bg_goals : List[(List[goal], List[goal])],
    shelved_goals : List[goal],
    given_up_goals : List[goal])

  type hint = List[(String, String)]

  type option_name = List[String]

  abstract class option_value
  case class BoolValue(
    value : Boolean) extends option_value
  case class IntValue(
    value : Option[Int]) extends option_value
  case class StringValue(
    value : String) extends option_value
  case class StringOptValue(
    value : Option[String]) extends option_value

  case class option_state(
    opt_sync : Boolean,
    opt_depr : Boolean,
    opt_name : String,
    opt_value : option_value)

  abstract class search_constraint
  case class Name_Pattern(
    value : String) extends search_constraint
  case class Type_Pattern(
    value : String) extends search_constraint
  case class SubType_Pattern(
    value : String) extends search_constraint
  case class In_Module(
    value : List[String]) extends search_constraint
  case class Include_Blacklist() extends search_constraint

  type search_flags = List[(search_constraint, Boolean)]

  case class coq_object[A](
    coq_object_prefix : List[String],
    coq_object_qualid : List[String],
    coq_object_object : A)

  case class coq_info(
    coqtop_version : String,
    protocol_version : String,
    release_date : String,
    compile_date : String)

  type location = Option[(Int, Int)]
  type state_id = Int
  type route_id = Int
  
  /* Obsolete(?) */
  type edit_id = Int

  abstract class value[A]
  case class Good[A](value : A) extends value[A]
  case class Fail[A](value : (state_id, location, String)) extends value[A]

  type union[A, B] = Either[A, B]

  object XML {
    object SimpleElem {
      def unapplySeq(n : Node) : Option[(String, Seq[Node])] =
        Elem.unapplySeq(n) match {
          case Some((prefix, label, attribs, scope, children)) =>
            Some((label, children))
          case _ =>
            None
        }
    }
    private def _attr(e : Elem, a : String) =
      Option(e \@ a).filterNot(_.isEmpty)
    private def _elch(e : Elem) = e.child.filter(_.isInstanceOf[Elem])

    def unwrapValue[A](a : Elem => A)(e : Elem) : value[A] =
      (_attr(e, "val"), _elch(e)) match {
        case (Some("good"), Seq(c : Elem)) =>
          Good(a(c))
        case (Some("good"), Seq()) =>
          Fail(-1, None, "Non-XML response body -- shouldn't happen!")
        case (Some("fail"), Seq(sid : Elem, error : Elem)) =>
          val loc = 
            (_attr(e, "loc_s"), _attr(e, "loc_e")) match {
              case (Some(start), Some(end)) =>
                Some((start.toInt, end.toInt))
              case _ =>
                None
            }
          Fail(unwrapStateId(sid), loc, unwrapString(error))
      }

    def wrapUnit() = <unit />
    def unwrapUnit(e : Elem) = ()
    
    def wrapString(a : CharSequence) = <string>{a}</string>
    def unwrapString(e : Elem) = e.text.trim()
  
    def wrapInt(a : Int) = <int>{a}</int>
    def unwrapInt(e : Elem) = unwrapString(e).toInt

    def wrapStateId(a : state_id) = <state_id val={a.toString} />
    def unwrapStateId(e : Elem) : state_id = _attr(e, "val").get.toInt

    def wrapRouteId(a : route_id) = <route_id val={a.toString} />

    def wrapBoolean(a : Boolean) = <bool val={a.toString} />
    def unwrapBoolean(e : Elem) =
      _attr(e, "val") match {
        case Some("true") => true
        case _ => false
      }

    def wrapList[A](a : A => Elem)(v : Seq[A]) =
      <list>{v.map(a)}</list>
    def unwrapList[A](a : Elem => A)(e : Elem) =
      e.child.map(_.asInstanceOf[Elem]).map(a).toList

    def wrapOption[A](a : A => Elem)(v : Option[A]) =
      v match {
        case Some(u) =>
          <option val="some">{a(u)}</option>
        case None =>
          <option val="none" />
      }
    def unwrapOption[A](a : Elem => A)(e : Elem) =
      _attr(e, "val") match {
        case Some("some") =>
          Some(a(e.child(0).asInstanceOf[Elem]))
        case _ =>
          None
      }
    def wrapPair[A, B](a : A => Elem, b : B => Elem)(v : (A, B)) =
      <pair>{a(v._1)}{b(v._2)}</pair>
    def unwrapPair[A, B](a : Elem => A, b : Elem => B)(e : Elem) =
      _elch(e) match {
        case Seq(c1 : Elem, c2 : Elem) =>
          (a(c1), b(c2))
      }

    def wrapUnion[A, B](a : A => Elem, b : B => Elem)(v : union[A, B]) =
      v match {
        case Left(l) =>
          <union val="in_l">{a(l)}</union>
        case Right(r) =>
          <union val="in_r">{b(r)}</union>
      }
    def unwrapUnion[A, B](a : Elem => A, b : Elem => B)(e : Elem) : Either[A, B] =
      (_attr(e, "val"), _elch(e)) match {
        case (Some("in_l"), Seq(c : Elem)) =>
          Left(a(c))
        case (Some("in_r"), Seq(c : Elem)) =>
          Right(b(c))
      }

    def unwrapStatus(e : Elem) : status = e match {
      case SimpleElem(
          "status", p : Elem, pname : Elem, ap : Elem, pn : Elem) =>
        status(unwrapList(unwrapString)(p),
            unwrapOption(unwrapString)(pname),
            unwrapList(unwrapString)(ap),
            unwrapInt(pn))
    }

    def unwrapCoqInfo(e : Elem) : coq_info = e match {
      case SimpleElem(
          "coq_info", ctv : Elem, prv : Elem, rd : Elem, cd : Elem) =>
        coq_info(unwrapString(ctv),
            unwrapString(prv),
            unwrapString(rd),
            unwrapString(cd))
    }

    def unwrapGoal(e : Elem) : goal = e match {
      case SimpleElem("goal", id : Elem, hyps : Elem, ccl : Elem) =>
        goal(unwrapString(id),
            unwrapList(unwrapString)(hyps),
            unwrapString(ccl))
    }

    def unwrapGoals(e : Elem) : goals = e match {
      case SimpleElem("goals",
          cur : Elem, bac : Elem, she : Elem, aba : Elem) =>
        goals(unwrapList(unwrapGoal)(cur),
            unwrapList(unwrapPair(
                unwrapList(unwrapGoal), unwrapList(unwrapGoal)))(bac),
            unwrapList(unwrapGoal)(she),
            unwrapList(unwrapGoal)(aba))
    }

    def wrapOptionValue(v : option_value) = v match {
      case BoolValue(b) =>
        <option_value val="boolvalue">{wrapBoolean(b)}</option_value>
      case IntValue(i) =>
        <option_value val="intvalue">{wrapOption(wrapInt)(i)}</option_value>
      case StringValue(s) =>
        <option_value val="stringvalue">{wrapString(s)}</option_value>
      case StringOptValue(s) =>
        <option_value val="stringoptvalue">{
          wrapOption(wrapString)(s)}</option_value>
    }
    def unwrapOptionValue(e : Elem) : option_value =
    (_attr(e, "val"), _elch(e)) match {
      case (Some("boolvalue"), Seq(e : Elem)) =>
        BoolValue(unwrapBoolean(e))
      case (Some("intvalue"), Seq(e : Elem)) =>
        IntValue(unwrapOption(unwrapInt)(e))
      case (Some("stringvalue"), Seq(e : Elem)) =>
        StringValue(unwrapString(e))
      case (Some("stringoptvalue"), Seq(e : Elem)) =>
        StringOptValue(unwrapOption(unwrapString)(e))
    }

    /* <option_state /> elements are never sent to Coq */
    def unwrapOptionState(e : Elem) : option_state =
      _elch(e) match {
        case Seq(s : Elem, d : Elem, n : Elem, v : Elem) =>
          option_state(
              unwrapBoolean(s),
              unwrapBoolean(d),
              unwrapString(n),
              unwrapOptionValue(v))
      }

    def unwrapHint(e : Elem) : hint =
      unwrapList(unwrapPair(unwrapString, unwrapString))(e)

    def wrapSearchConstraint(v : search_constraint) = v match {
      case Name_Pattern(p) =>
        <search_cst val="name_pattern">{wrapString(p)}</search_cst>
      case Type_Pattern(p) =>
        <search_cst val="type_pattern">{wrapString(p)}</search_cst>
      case SubType_Pattern(p) =>
        <search_cst val="subtype_pattern">{wrapString(p)}</search_cst>
      case In_Module(p) =>
        <search_cst val="in_module">{wrapList(wrapString)(p)}</search_cst>
      case Include_Blacklist() =>
        <search_cst val="include_blacklist" />
    }
    def unwrapCoqObject[A](a : Elem => A)(e : Elem) : coq_object[A] =
      _elch(e) match {
        case Seq(m : Elem, n : Elem, d : Elem) =>
          coq_object(
              unwrapList(unwrapString)(m), unwrapList(unwrapString)(n), a(d))
      }

    def _unwrapRaw(e : Elem) = e
  }
}