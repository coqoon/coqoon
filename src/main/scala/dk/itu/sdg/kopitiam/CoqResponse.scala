/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

sealed abstract class CoqResponse { }

case class CoqShellReady (mono : Boolean, tokens : CoqShellTokens) extends CoqResponse { }
