/* (c) 2010-2011 Hannes Mehnert and David Christiansen */

package dk.itu.coqoon.ui.coqparser

import dk.itu.coqoon.ui.parsing._

trait VernacularRegion extends LengthPositional with Product {
  val outline = false
  def outlineName = "<no name>"
  def outlineNameExtra = "" //pos.toString
  override def toString = "<" + outlineName + " " + outlineNameExtra + ">"

  def getOutline = subRegions.filter(_.outline)

  lazy val subRegions : Stream[VernacularRegion] = subRegions(productIterator.toStream)
  private def subRegions (fields : Stream[Any]) : Stream[VernacularRegion] =
    fields match {
      case (field : VernacularRegion) #:: rest => field #:: subRegions(rest)
      case (field : Traversable[VernacularRegion]) #:: rest => field.toStream ++ subRegions(rest)
      case _ #:: rest => subRegions(rest)
      case Stream.Empty => Stream.Empty
    }

  lazy val hasSubRegions : Boolean = subRegions.length > 0
}

trait VernacularReserved {
  // Not technically reserved words, but they work as such. Used to start top-level forms.
  val keyword = """Axiom Conjecture Parameter Parameters Variable Variables Hypothesis
                   Hypotheses Definition Example Inductive CoInductive Fixpoint CoFixpoint
                   Program Goal Let Remark Fact Corollary Proposition Lemma Instance Theorem Tactic
                   Ltac Notation Infix Add Record Section Module Require Import Export Open
                   Proof End Qed Admitted Save Defined Print Eval Check Hint""".split("""\s+""").toList

  val operator = List("!", "%", "&", "&&", "(", "()", ")",
                      "*", "+", "++", ",", "-", "->", ".",
                      ".(", "..", "/", "/\\", ":", "::", ":<",
                      ":=", ":>", ";", "<", "<-", "<->", "<:",
                      "<=", "<>", "=", "=>", "=_D", ">", ">->",
                      ">=", "?", "?=", "@", "[", "\\/", "]",
                      "^", "{", "|", "|-", "||", "}", "~")

  // The reserved words as listed in the reference manual
  val keywords = List("_", "as", "at", "cofix", "else", "end",
                      "exists", "exists2", "fix", "for", "forall", "fun",
                      "if", "IF", "in", "let", "match", "mod",
                      "Prop", "return", "Set", "then", "Type", "using",
                      "where", "with")

  val proofEnders = List("End", "Qed", "Admitted", "Defined", "Save", "Proof term")
}

