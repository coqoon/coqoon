/* CoqDocumentAssistants.scala
 * Classes for configuring and partitioning Coq documents
 * Copyright © 2013, 2014 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.rules.{
  IPartitionTokenScanner, BufferedRuleBasedScanner}

object CoqPartitions {
  final val COQ = "__coqoon_ptn_coqn"
  object Types {
    final val COQ = "__coqoon_ptn_coqn_coq"
    final val STRING = "__coqoon_ptn_coqn_string"
    final val COMMENT = "__coqoon_ptn_coqn_comment"
  }
  final val TYPES = Array(Types.COQ, Types.COMMENT, Types.STRING)

  import org.eclipse.ui.editors.text.ForwardingDocumentProvider
  import org.eclipse.jface.text.{IDocumentExtension3, IDocumentPartitioner}
  import org.eclipse.jface.text.rules.FastPartitioner
  import org.eclipse.core.filebuffers.IDocumentSetupParticipant

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

  def createPartitioner() : IDocumentPartitioner =
    new FastPartitioner(new CoqPartitionScanner, CoqPartitions.TYPES)
}

class CoqPartitionScanner extends IPartitionTokenScanner {
  private var document : Option[IDocument] = None
  private var contentType : Option[String] = None

  override def setRange(document : IDocument, offset : Int, length : Int) = {
    println(s"${this}.setRange(${document}, ${offset}, ${length})")
    this.document = Option(document)
    this.start = offset
    this.position = offset
    this.end = offset + length
  }

  override def setPartialRange(document : IDocument, offset : Int,
      length : Int, contentType : String, partitionOffset : Int) : Unit = {
    println(s"${this}.setPartialRange(${document}, " +
        s"${offset}, ${length}, ${contentType}, ${partitionOffset})")
    if (contentType != null && partitionOffset != -1) {
      lastFinalState = contentType match {
        case CoqPartitions.Types.COQ => States.Coq
        case CoqPartitions.Types.COMMENT => States.Comment
        case CoqPartitions.Types.STRING => States.String
        case _ => States.Coq
      }
      lastEnd = partitionOffset
    }
    setRange(document, offset, length)
  }

  private var (start, position, end) = (0, 0, 0)

  import org.eclipse.jface.text.rules.{Token, IToken}

  import States._

  private var lastFinalState : PartitionFinalState = Coq

  private var (lastStart, lastEnd) = (-1, 0)

  override def nextToken() : IToken = {
    val doc = document.get
    if (position == end) {
      lastStart = doc.getLength
      lastEnd = lastStart
      return Token.EOF
    }
    position += lastFinalState.offsetCorrection
    var state : PartitionState = lastFinalState
    while (position < end) {
      val c = doc.getChar(position)
      state.get(c) match {
        case Some(f : PartitionFinalState)
            if f != lastFinalState =>
          position += 1 - f.offsetCorrection()
          try {
            lastStart = lastEnd
            lastEnd = position
            if (lastEnd - lastStart > 0) {
              println("Yielding " + lastFinalState + " (from "
                  + lastStart + " to " + lastEnd + ")")
              return lastFinalState.getToken.get
            } else println("Not yielding " + lastFinalState +
                " (length " + (lastEnd - lastStart) + ")")
          } finally {
            lastFinalState = f
          }
        case Some(s : PartitionState) =>
          state = s
        case _ =>
      }
      position += 1
    }
    println("Yielding fallback " + lastFinalState)
    lastStart = lastEnd
    lastEnd = position
    lastFinalState.getToken.get
  }

  override def getTokenOffset() = lastStart
  override def getTokenLength() = lastEnd - lastStart
}

private object States {
  import org.eclipse.jface.text.rules.{Token, IToken}
  sealed abstract class PartitionState extends BasicRule.State
  sealed abstract class PartitionFinalState(
      token : IToken) extends PartitionState {
    setToken(token)
    def offsetCorrection() : Int
  }

  case object Coq extends PartitionFinalState(
      new Token(CoqPartitions.Types.COQ)) {
    add('"', String)
    add('(', PossiblyEnteringComment)
    setFallback(Coq)

    override def offsetCorrection = 0
  }

  case object String extends PartitionFinalState(
      new Token(CoqPartitions.Types.STRING)) {
    add('\\', StringEscape)
    add('"', Coq)
    setFallback(String)

    override def offsetCorrection = 1
  }

  case object StringEscape extends PartitionState {
    setFallback(String)
  }

  case object PossiblyEnteringComment extends PartitionState {
    add('"', String)
    add('*', Comment)
    add('(', PossiblyEnteringComment)
    setFallback(Coq)
  }

  case object Comment extends PartitionFinalState(
      new Token(CoqPartitions.Types.COMMENT)) {
    add('*', PossiblyLeavingComment)
    setFallback(Comment)

    override def offsetCorrection = 2
  }

  case object PossiblyLeavingComment extends PartitionState {
    add('*', PossiblyLeavingComment)
    add(')', Coq)
    setFallback(Comment)
  }

  import org.eclipse.jface.text.rules.ICharacterScanner
  Seq(Coq, String, Comment).foreach(_.add(ICharacterScanner.EOF, End))

  case object End extends PartitionFinalState(Token.EOF) {
    setFallback(End)

    override def offsetCorrection = 0
  }
}