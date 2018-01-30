package dk.itu.coqoon.ui.text

import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.{IRegion, ITypedRegion}
import org.eclipse.jface.text.{DocumentEvent, IDocumentPartitioner,
  DocumentRewriteSession, IDocumentPartitionerExtension,
  IDocumentPartitionerExtension2, IDocumentPartitionerExtension3}

class TransitionPartitioner(
    t : PushdownAutomaton[Char], start : PushdownAutomaton.State,
    rules : Seq[TransitionPartitioner.Rule], defaultLabel : String)
    extends IDocumentPartitioner
        with IDocumentPartitionerExtension
        with IDocumentPartitionerExtension2
        with IDocumentPartitionerExtension3 {
  private var document : Option[IDocument] = None
  private val tracker = new TransitionTracker(t, start)
  private var regions : Array[ITypedRegion] = Array()

  def connect(d : IDocument, delayInitialisation : Boolean) = {
    document = Option(d)
    tracker.update(0, 0, d.getLength, d.getChar, d.getLength)
    recomputeRegions(tracker.getExecutions)
  }
  def disconnect() = {
    regions = Array()
    tracker.clear
    document = None
  }

  def documentAboutToBeChanged(ev : DocumentEvent): Unit = (/* Hooray! */) 
   
  def getLegalContentTypes(): Array[String] = ??? 

  def documentChanged2(ev : DocumentEvent) : IRegion = {
    val r = tracker.update(
        ev.fOffset, ev.fLength, ev.fText.length, ev.fDocument)
    recomputeRegions(tracker.getExecutions)
    r
  }
    
  import TransitionPartitioner.{Rule, Transition}
  private def recomputeRegions(s_ : Seq[Transition]) = {
    var transitions = s_

    var regions = Seq[(String, Int)]()
    var rSize = 0
    var rType = defaultLabel
    
    while (!transitions.isEmpty) {
      import TransitionPartitioner.startsWith2
      rules.collectFirst {
        case r @ Rule(leadIn, _, _, _)
            if startsWith2[Transition, PushdownAutomaton.State](transitions, _._1.position, leadIn) =>
          r
      } match {
        case Some(Rule(_, leadOut, leadOutLength, label)) =>
          if (rSize >= 0)
            regions :+= (rType, rSize)
          rSize = 0
          rType = label
          import scala.util.control.Breaks.{break, breakable}
          breakable {
            while (!transitions.isEmpty) {
              if (startsWith2[Transition, PushdownAutomaton.State](transitions, _._1.position, leadOut)) {
                val (dropped, left) = transitions.splitAt(leadOut.length - 1)
                rSize += dropped.map(_._2).sum
                transitions = left
                break
              } else {
                rSize += transitions.head._2
                transitions = transitions.tail
              }
            }
          }
          val actualLength =
            if (!transitions.isEmpty) {
              rSize + leadOutLength
            } else rSize
          regions :+= (rType, actualLength)
          rSize = -leadOutLength
          rType = defaultLabel
        case None =>
          rSize += transitions.head._2
          transitions = transitions.tail
      }
    }
    if (rSize >= 0)
      regions :+= (rType, rSize)
    var pos = 0
    this.regions = regions.map {
      case (label, length) =>
        try {
          Region(pos, length = length).asTypedRegion(label)
        } finally pos += length
    }.toArray
    this.regions
  }
  
  def computePartitioning(offset : Int,
      length : Int, zeroLength : Boolean) : Array[ITypedRegion] = {
    val r = Region(offset, length = length)
    for (part <- this.regions if part.getLength > 0 || zeroLength;
         _ <- r.intersection(Region(part.getOffset, length = part.getLength)))
      yield part
  }

  def getContentType(offset : Int, preferOpen : Boolean) : String =
    Option(getPartition(offset, preferOpen)).map(_.getType).orNull

  def getManagingPositionCategories(): Array[String] = null 
  def getPartition(offset : Int, preferOpen : Boolean) : ITypedRegion =
    regions find {
      case r if offset >= r.getOffset && !preferOpen && offset < r.getOffset + r.getLength =>
        true
      case r if offset >= r.getOffset && preferOpen && offset <= r.getOffset + r.getLength =>
        true
      case _ => false
    } match {
      case Some(r) => r
      case None =>
        /* Presumably this can only happen at the end of the document? */
        Region(offset, length = 0).asTypedRegion(
            regions.lastOption.map(_.getType).getOrElse(defaultLabel))
    }

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
}
object TransitionPartitioner {
  def startsWith2[A, B](a : Seq[A], ac : A => B, b : Seq[B]) : Boolean = {
    a.zip(b) foreach {
      case (a, b)
          if ac(a) != b =>
        return false
      case _ =>
    }
    return true
  }
  type Transition = (PushdownAutomaton[Char]#Execution, Int)
  case class Rule(
      leadIn : Seq[PushdownAutomaton.State],
      leadOut : Seq[PushdownAutomaton.State],
      leadOutLength : Int, label : String)
}