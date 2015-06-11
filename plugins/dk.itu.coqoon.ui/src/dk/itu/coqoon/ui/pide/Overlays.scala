package dk.itu.coqoon.ui.pide

import isabelle.Command

case class Overlay(
    command : Command, operation : String, arguments : List[String]) {
  lazy val id = isabelle.Document_ID.make()

  def insert(o : isabelle.Document.Node.Overlays) =
    o.insert(command, operation, id.toString +: arguments)
  def wrap() = insert(isabelle.Document.Node.Overlays.empty)
}

object Queries {
  object coq_query {
    def apply(command : Command, query : String) =
      Overlay(command, "coq_query", List(query))
  }
}