/* TokeniserPartitioner.scala
 * An IDocument partitioner that maps Tokeniser tokens to document partitions
 * Copyright © 2015 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.ui.text

import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.{IRegion, ITypedRegion}
import org.eclipse.jface.text.{DocumentEvent, IDocumentPartitioner,
  IDocumentPartitionerExtension, IDocumentPartitionerExtension2,
  IDocumentPartitionerExtension3}

import dk.itu.coqoon.core.utilities.CacheSlot

object DocumentAdapter {
  class DocumentSequence(
      d : IDocument, start : Int, end : Int) extends CharSequence {
    final lazy val length = end - start
    override def charAt(pos : Int) = d.getChar(start + pos)
    override def subSequence(start : Int, end : Int) =
      new DocumentSequence(d, this.start + start, this.start + end)
    override def toString = d.get(start, length)
    def describe = s"DocumentSequence($d, start = $start, end = $end)"
  }
  def makeSequence(d : IDocument) : DocumentSequence =
    new DocumentSequence(d, 0, d.getLength)
}

class TokeniserPartitioner(
    t : Tokeniser, start : PushdownAutomaton.State,
    mapping : Map[PushdownAutomaton.State,
      (String /* partition type */, Int /* length of opening delimiter */)])
    extends IDocumentPartitioner
        with IDocumentPartitionerExtension
        with IDocumentPartitionerExtension2
        with IDocumentPartitionerExtension3 {
  private val tokens = CacheSlot {
    t.tokens(start, DocumentAdapter.makeSequence(this.document.get)).toList
  }

  def getTokens() = tokens.get

  /* Defer to the IDocumentPartitionerExtension version of this method */
  override def documentChanged(ev : DocumentEvent) =
    (documentChanged2(ev) != null)
  /* Defer to the IDocumentPartitionerExtension2 versions of these methods */
  override def computePartitioning(
      offset : Int, length : Int) = computePartitioning(offset, length, false)
  override def getPartition(offset : Int) = getPartition(offset, false)
  override def getContentType(offset : Int) = getContentType(offset, false)
  /* Defer to the IDocumentPartitionerExtension3 version of this method */
  override def connect(document : IDocument) : Unit = connect(document, false)

  private var document : Option[IDocument] = None

  override def connect(
      document : IDocument, delayInitialisation : Boolean) : Unit = {
    this.document = Option(document)
    if (!delayInitialisation)
      getTokens()
  }
  override def disconnect() : Unit = {
    this.document = None
    tokens.clear
  }

  import org.eclipse.jface.text.DocumentRewriteSession
  private var session : Option[DocumentRewriteSession] = None
  override def startRewriteSession(session : DocumentRewriteSession) =
    this.session match {
      case Some(s) =>
        throw new IllegalStateException("Rewrite session already underway")
      case None =>
        this.session = Option(session)
    }
  override def stopRewriteSession(session : DocumentRewriteSession) =
    this.session match {
      case Some(s) if s == session =>
        this.session = None
      case _ =>
    }
  override def getActiveRewriteSession() = session.orNull

  private def scrutiniseToken(d : IDocument, ts : List[Tokeniser.Token],
      idx : Int, t0 : Tokeniser.Token, pos : Int) : IRegion = {
    val (t, s) = t0
    val tr = Region(pos, length = s.length)
    val Some((_, leadin)) = mapping.get(t)
    val next =
      if (idx < (ts.length - 1)) {
        Some(ts(idx + 1))
      } else None
    val subsequentTokens = {
      val rs = tr.start + leadin
      this.t.tokens(t,
          DocumentAdapter.makeSequence(d).subSequence(rs, d.getLength))
    }

    val (t1, t2) =
      if (subsequentTokens.hasNext) {
        val t1_ = subsequentTokens.next
        /* The first token won't include its leadin sequence, so add it back */
        val t1 = (t1_._1, d.get(tr.start, leadin) + t1_._2)
        if (subsequentTokens.hasNext) {
          (Some(t1), Some(subsequentTokens.next))
        } else (Some(t1), None)
      } else (None, None)

    if (t1 == None) {
      tokens.clear
    } else if (t2 == next) {
      /* Hooray! The next token wasn't affected by this change, so we can just
       * swap out this token without redoing everything after this point */
      if (t0 == t1.get) {
        /* In fact, this change doesn't seem to have done anything at all, so
         * let's just suppress it entirely */
        return null
      } else tokens.set(Some(ts.updated(idx, t1.get)))
    } else {
      /* XXX: can we do something cleverer when we introduce a new token at
       * the end of the document? */
      if (idx != 0) {
        tokens.set(Some(ts.take(idx) ++ t1 ++ t2 ++ subsequentTokens.toList))
      } else tokens.clear
    }
    Region(pos, length = d.getLength - pos)
  }

  override def documentAboutToBeChanged(ev: DocumentEvent) : Unit = ()
  override def documentChanged2(ev : DocumentEvent) : IRegion =
    tokens.asOption match {
      case Some(ts) =>
        var pos = 0
        ts.zipWithIndex foreach {
          case (t0 @ (t, s), idx)
              if Region(pos, length = s.length).contains(ev.fOffset) =>
            /* This is the affected region; see if we can get away with
             * patching it out of the existing list of tokens */
            val Some((_, leadin)) = mapping.get(t)
            if (Region(pos, length = leadin).contains(ev.fOffset)) {
              /* Oh no! The lead-in sequence may have been damaged! Let's try
               * to recover by pretending that the /last/ token was the
               * affected one */
              val tP = ts(idx - 1)
              return scrutiniseToken(
                  ev.fDocument, ts, idx - 1, tP, pos - tP._2.length)
            } else return scrutiniseToken(ev.fDocument, ts, idx, t0, pos)
          case ((_, s), _) =>
            pos += s.length
        }
        /* If we've made it through all the tokens, then we're presumably at
         * the end of the document; handle this change by examining the last
         * token */
        val tP = ts.last
        scrutiniseToken(
            ev.fDocument, ts, ts.length - 1, tP, pos - tP._2.length)
      case None =>
        /* To err on the safe side, claim that everything's changed */
        Region(0, length = ev.fDocument.getLength)
    }

  private final lazy val contentTypes = mapping.values.map(_._1).toSet.toArray
  override def getLegalContentTypes() = contentTypes

  override def computePartitioning(offset : Int, length : Int,
      includeZeroLengthPartitions : Boolean) : Array[ITypedRegion] = {
    var (i, end) = (offset, offset + length)
    var s = Seq[ITypedRegion]()
    while (i < end) {
      val p = getPartition(i, false)
      if ((includeZeroLengthPartitions || p.getLength > 0) &&
          !s.contains(p))
        s :+= p
      i = p.getOffset + p.getLength
    }
    s.toArray
  }
  override def getContentType(offset : Int,
      preferOpenPartitions : Boolean) : String =
    getPartition(offset, preferOpenPartitions).getType
  override def getManagingPositionCategories() : Array[String] = {
    null /* XXX: should we stash the partition information in the document? */
  }
  override def getPartition(
      offset : Int, preferOpenPartitions : Boolean) : ITypedRegion = {
    val length = document.map(_.getLength).get
    var pos = 0
    for ((t, s) <- getTokens;
         (label, leadin) <- mapping.get(t)) {
      val tr = Region(pos, length = s.length)
      if (tr.contains(offset) ||
          (preferOpenPartitions && leadin == 0 && tr.end == offset) ||
          length == offset) {
        return tr.asTypedRegion(label)
      } else pos = tr.end
    }
    return Region(offset,
        length = length - offset).asTypedRegion(mapping.get(start).get._1)
  }
}
