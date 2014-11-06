/* CoqDocumentAssistants.scala
 * Classes for configuring and partitioning Coq documents
 * Copyright © 2013, 2014 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.core.model.StateRule
import org.eclipse.jface.text.rules.IToken

sealed abstract class PartitionState
      extends StateRule.TokenState[Char, IToken, PartitionState]
          with StateRule.FallbackState[Char, PartitionState]
sealed abstract class PartitionFinalState(
    token : IToken) extends PartitionState {
  setToken(Option(token))

  /* Returns the length of the character sequence that leads into this state.
   * (This value is used to make sure that lead-in sequences correctly
   * terminate a partition without becoming part of it.) */
  def getLeadCount() : Int
}

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

abstract class PartitionStateScanner(
    default : PartitionFinalState) extends IPartitionTokenScanner {
  def convertContentType(ct : String) : PartitionFinalState

  private var document : Option[IDocument] = None
  private var contentType : Option[String] = None

  override def setRange(document : IDocument, offset : Int, length : Int) = {
    this.document = Option(document)
    this.start = offset
    this.position = offset
    this.end = offset + length
    lastEnd = 0
    lastStart = 0
    lastFinalState = default
  }

  override def setPartialRange(document : IDocument, offset : Int,
      length : Int, contentType : String, partitionOffset : Int) : Unit = {
    setRange(document, offset, length)
    if (contentType != null && partitionOffset != -1) {
      lastFinalState = convertContentType(contentType)
      lastEnd = partitionOffset
      /* If we're at the start of this partition, skip lead-in characters */
      if (offset == partitionOffset)
        position += lastFinalState.getLeadCount
    }
  }

  private var (start, position, end) = (0, 0, 0)

  import org.eclipse.jface.text.rules.{Token, IToken}

  private var lastFinalState : PartitionFinalState = default

  private var (lastStart, lastEnd) = (-1, 0)

  override def nextToken() : IToken = {
    val doc = document.get
    if (position >= end) {
      return (lastFinalState.getToken match {
        case Some(t) if lastEnd != end =>
          /* We do actually have a (partial) token ready, so return it */
          lastStart = lastEnd
          lastEnd = end
          t
        case _ =>
          /* No characters left and no partial token to return. We're
           * officially done */
          Token.EOF
      })
    }
    var state : PartitionState = lastFinalState
    while (position < end) {
      val c = doc.getChar(position)
      state.get(Some(c)) match {
        case Some(f : PartitionFinalState)
            if f != lastFinalState =>
          position += 1
          try {
            lastStart = lastEnd
            lastEnd = position - f.getLeadCount
            if (lastEnd - lastStart > 0)
              return lastFinalState.getToken.get
          } finally {
            state = f
            lastFinalState = f
          }
        case Some(s : PartitionState)
            if state != s =>
          state = s
          position += 1
        case _ =>
          position += 1
      }
    }
    lastStart = lastEnd
    lastEnd = position
    lastFinalState.getToken.get
  }

  override def getTokenOffset() = lastStart
  override def getTokenLength() = lastEnd - lastStart
}

class CoqPartitionScanner
    extends PartitionStateScanner(CoqPartitionScanner.Coq) {
  import CoqPartitionScanner._

  override def convertContentType(ct : String) =
    ct match {
      case CoqPartitions.Types.COQ => Coq
      case CoqPartitions.Types.COMMENT => Comment
      case CoqPartitions.Types.STRING => String
      case _ => Coq
    }
}
object CoqPartitionScanner {
  import org.eclipse.jface.text.rules.Token

  case object Coq extends PartitionFinalState(
      new Token(CoqPartitions.Types.COQ)) {
    add(Some('"'), String)
    add(Some('('), PossiblyEnteringComment)
    setFallback(Some(Coq))

    override def getLeadCount = 0
  }

  case object String extends PartitionFinalState(
      new Token(CoqPartitions.Types.STRING)) {
    add(Some('\\'), StringEscape)
    add(Some('"'), Coq)
    setFallback(Some(String))

    override def getLeadCount = 1
  }

  case object StringEscape extends PartitionState {
    setFallback(Some(String))
  }

  case object PossiblyEnteringComment extends PartitionState {
    add(Some('"'), String)
    add(Some('*'), Comment)
    add(Some('('), PossiblyEnteringComment)
    setFallback(Some(Coq))
  }

  case object Comment extends PartitionFinalState(
      new Token(CoqPartitions.Types.COMMENT)) {
    add(Some('*'), PossiblyLeavingComment)
    setFallback(Some(Comment))

    override def getLeadCount = 2
  }

  case object PossiblyLeavingComment extends PartitionState {
    add(Some('*'), PossiblyLeavingComment)
    add(Some(')'), Coq)
    setFallback(Some(Comment))
  }

  import org.eclipse.jface.text.rules.ICharacterScanner
  Seq(Coq, String, Comment).foreach(_.add(None, End))

  case object End extends PartitionFinalState(Token.EOF) {
    setFallback(Some(End))

    override def getLeadCount = 0
  }
}
