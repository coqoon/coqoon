/* CoqPartitions.scala
 * Utility methods for installing Coq partitioning support
 * Copyright © 2013, 2014, 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

object CoqPartitions {
  final val ID = "__coqoon_coq"
  object Types {
    final val COQ = s"${ID}_coq"
    final val STRING = s"${COQ}_string"
    final val COMMENT = s"${COQ}_comment"
  }
  final lazy val TYPES = Array(Types.COQ, Types.STRING, Types.COMMENT)

  import dk.itu.coqoon.ui.text.coq.{CoqTokeniser, CoqRecogniser}
  private[CoqPartitions] lazy val tokeniserPartitionerMapping = {
    import CoqRecogniser.States._
    Map(coq -> (Types.COQ, 0),
        coqString -> (Types.STRING, 1),
        coqComment -> (Types.COMMENT, 2))
  }

  import dk.itu.coqoon.ui.utilities.ProfilingProxy
  import org.eclipse.jface.text.{IDocument, IDocumentExtension3}
  import org.eclipse.jface.text.{
    IDocumentPartitioner, IDocumentPartitionerExtension}
  def installPartitioner(input : IDocument, partitioning : String) = {
    import dk.itu.coqoon.core.utilities.TryCast
    val partitioner =
      ProfilingProxy[IDocumentPartitioner with IDocumentPartitionerExtension](
          createPartitioner, "documentChanged2")
    TryCast[IDocumentExtension3](input) match {
      case Some(ext) =>
        ext.setDocumentPartitioner(partitioning, partitioner)
      case None =>
        input.setDocumentPartitioner(partitioner)
    }
    partitioner.connect(input)
  }

  private[CoqPartitions] lazy val transitionPartitionerMapping = {
    import dk.itu.coqoon.ui.text.TransitionPartitioner.Rule
    import CoqRecogniser.States._
    Seq(Rule(leadIn = Seq(nearlyCoqComment, coqComment),
             leadOut = Seq(nearlyOutOfCoqComment, coq),
             1, Types.COMMENT),
        Rule(leadIn = Seq(coqString),
             leadOut = Seq(coq),
             1, Types.STRING))
  }

  import dk.itu.coqoon.ui.CoqoonUIPreferences
  import dk.itu.coqoon.ui.text.{TokeniserPartitioner, TransitionPartitioner}
  def createPartitioner() =
    CoqoonUIPreferences.Partitioner.get match {
      case "token" =>
        new TokeniserPartitioner(CoqTokeniser,
            CoqRecogniser.States.coq, tokeniserPartitionerMapping)
      case _ =>
        new TransitionPartitioner(CoqRecogniser,
            CoqRecogniser.States.coq, transitionPartitionerMapping, Types.COQ)
  }
}
