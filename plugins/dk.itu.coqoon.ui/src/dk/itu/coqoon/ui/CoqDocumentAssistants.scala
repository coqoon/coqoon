/* CoqDocumentAssistants.scala
 * Classes for configuring and partitioning Coq documents
 * Copyright © 2013, 2014 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import org.eclipse.jface.text.IDocument

object CoqPartitions {
  final val COQ = "__coqoon_ptn_coqn"
  object Types {
    final val COQ = "__coqoon_ptn_coqn_coq"
    final val STRING = "__coqoon_ptn_coqn_string"
    final val COMMENT = "__coqoon_ptn_coqn_comment"
  }
  final val TYPES = Array(Types.COQ, Types.COMMENT, Types.STRING)

  import org.eclipse.jface.text.{IDocumentExtension3, IDocumentPartitioner}

  def installPartitioner(input : IDocument, partitioning : String) = {
    import dk.itu.coqoon.core.utilities.TryCast
    val partitioner = createPartitioner
    TryCast[IDocumentExtension3](input) match {
      case Some(ext) =>
        ext.setDocumentPartitioner(partitioning, partitioner)
      case None =>
        input.setDocumentPartitioner(partitioner)
    }
    partitioner.connect(input)
  }

  import dk.itu.coqoon.ui.text.{Tokeniser, TokeniserPartitioner}
  import dk.itu.coqoon.ui.text.coq.{CoqTokeniser, CoqRecogniser}
  def createPartitioner() : IDocumentPartitioner =
    new TokeniserPartitioner(CoqTokeniser, CoqRecogniser.States.coq, mapping)

  private[CoqPartitions] val mapping = {
    import CoqRecogniser.States._
    Map(coq -> Types.COQ,
        coqString -> Types.STRING,
        coqComment -> Types.COMMENT)
  }
}
