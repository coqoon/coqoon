/* Overlays.scala
 * Classes and factories for creating and evaluating PIDE overlays
 * Copyright © 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.editors.pide

import isabelle.Command

case class Overlay(
    command : Command, operation : String, arguments : List[String]) {
  lazy val id = isabelle.Document_ID.make()

  def insert(o : isabelle.Document.Node.Overlays) =
    o.insert(command, operation, id.toString +: arguments)
  def wrap() = insert(isabelle.Document.Node.Overlays.empty)
}

trait OverlayRunner {
  def setOverlay(overlay : Option[(Overlay, OverlayListener)])
}

trait OverlayListener {
  def onResult(result : Either[String, Seq[String]])
}

object Queries {
  object coq_query {
    def apply(command : Command, query : String) =
      Overlay(command, "coq_query", List(query))
  }
}
