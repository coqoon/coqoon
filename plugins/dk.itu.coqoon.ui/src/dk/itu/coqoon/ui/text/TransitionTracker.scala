package dk.itu.coqoon.ui.text

import org.eclipse.jface.text.{IRegion, IDocument}

class TransitionTracker(
    val automaton : PushdownAutomaton[Char],
    val start : PushdownAutomaton.State) {
  import dk.itu.coqoon.ui.text.PushdownAutomaton
  import scala.collection.mutable.ListBuffer
  private var trace : ListBuffer[(automaton.Execution, Int)] = ListBuffer()

  def update(offset : Int,
      length : Int, replacedBy : Int, document : String) : IRegion =
    update(offset, length, replacedBy, document.charAt, document.length)

  def update(offset : Int,
      length : Int, replacedBy : Int, document : IDocument) : IRegion =
    update(
        offset, length, replacedBy, document.getChar, document.getLength)

  def update(offset : Int, length : Int, replacedBy : Int,
      charAt : Int => Char, totalLength : Int) : IRegion = {
    var traceFragment : ListBuffer[(automaton.Execution, Int)] = ListBuffer()

    /* If we already have a view of the executions in the document, then
     * annotate it with their indices and offsets (as a stream to minimise
     * unnecessary computation). */
    val ts = trace.toStream
    val tfs = {
      var idx = 0
      var count = 0
      ts map {
        case (e, t) =>
          try {
            (idx, e, t, count)
          } finally {
            idx += 1
            count += t
          }
      }
    }

    /* Work out which execution in the old document we should begin evaluation
     * at... */
    val beginAt = tfs.collectFirst {
      case q @ (i, e, s, o)
          if offset >= o && offset <= (o + s) =>
        q
    }
    /* ... and what execution we'll be in after the edit. */
    val afterEdit = tfs.collectFirst {
      case q @ (i, e, s, o)
          if (offset + length) >= o && (offset + length) <= (o + s) =>
        q
    }
    val initialExec = beginAt match {
      case Some((_, e, _, _)) =>
        e
      case None =>
        automaton.Execution(start, Seq())
    }

    /* oldState is the execution state (and its index, length and offset) at
     * offset oldPosition in the old document.*/
    var oldState = afterEdit
    var oldPosition = offset + length

    /* newExec is the current execution state we're building up, and
     * newExecLength is its length. (If we're resuming in an old execution
     * state, the length will start off as something other than 0.) */
    var newExec = initialExec
    var newExecLength = beginAt match {
      case Some((_, _, s, o)) =>
        offset - o
      case _ =>
        0
    }
    var position = offset

    import scala.util.control.Breaks.{break, breakable}
    breakable {
      while (position < totalLength) {
        if (position >= offset + replacedBy) {
          /* Synchronisation is now possible: when the old document state and
           * the new are the same, stop execution at once */
          oldState foreach {
            case o @ (oi, oe, os, ol)
                if oe == newExec =>
              /* The old machine and the new one are back in sync: fix up the
               * length of this execution state and stop evaluating things.*/
                newExecLength += ol - oldPosition + os
                break
            case o @ (oi, oe, os, ol) =>
              if (oldPosition >= (os + ol)) {
                /* We got to the end of this region; advance to the next one */
                oldState =
                  if (tfs.isDefinedAt(oi + 1)) {
                    Some(tfs(oi + 1))
                  } else None
              }
              oldPosition += 1
            case _ =>
          }
        }
        /* Feed the new document into the recogniser, noting when the execution
         * state changes. (Trivial transitions, like going from a state to
         * itself without modifying the stack, can be safely ignored.) */
        newExec.accept(charAt(position)) match {
          case Some((_, e)) if e == newExec =>
            newExecLength += 1
          case Some((t, e)) =>
            if (newExecLength != 0)
              traceFragment.append((newExec, newExecLength))
            newExec = e
            newExecLength = 1
          case _ =>
            /* The automaton rejected the character at this position, but this
             * should never happen in a tokenisable document */
            ???
        }
        position += 1
      }
    }
    if ((position - offset) != 0 || length != replacedBy) {
      /* If the old and the new documents didn't immediately sync on the first
       * character, or if the length of the document has changed, the
       * document's partitioning has changed and should be updated. */
      if (newExecLength != 0)
        traceFragment.append((newExec, newExecLength))

      val firstIndex = beginAt.map(_._1).getOrElse(0)
      val lastIndex = oldState.map(_._1 + 1).getOrElse(trace.length)
      if (this.trace.view(firstIndex, lastIndex) != traceFragment) {
        this.trace.remove(firstIndex, (lastIndex - firstIndex))
        this.trace.insertAll(firstIndex, traceFragment)
        Region(offset, length = position - offset)
      } else {
        /* Replacing a fragment with itself is a contentless change */
        Region(offset, length = 0)
      }
    } else Region(offset, length = 0)
  }
  def clear() = trace.clear

  /* Returns the live sequence of executions and lengths used as the internal
   * document representation. */
  def getExecutions() = trace
}