package dk.itu.coqoon.core.coqtop

object CoqTypes {
  /* This is just a Scala translation of lib/interface.mli */
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
    status_statenum : Int,
    status_proofnum : Int)

  case class goals(
    fg_goals : List[goal],
    bg_goals : List[(List[goal], List[goal])])

  case class hint(
    fg_goals : List[(String, String)])

  type option_name = List[String]

  abstract class option_value
  case class BoolValue(
    value : Boolean) extends option_value
  case class IntValue(
    value : Option[Int]) extends option_value
  case class StringValue(
    value : String) extends option_value

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

  abstract class message_level
  case class Debug(value : String) extends message_level
  case class Info() extends message_level
  case class Notice() extends message_level
  case class Warning() extends message_level
  case class Error() extends message_level

  case class message(
    message_level : message_level,
    message_content : String)

  type location = Option[(Int, Int)]

  abstract class value[A]
  case class Good[A](value : A) extends value[A]
  case class Unsafe[A](value : A) extends value[A]
  case class Fail[A](value : (location, String)) extends value[A]
}
