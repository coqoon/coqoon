package dk.itu.coqoon.ui.text

import org.eclipse.jface.text.IDocument

class TransitionTracker(
    val automaton : PushdownAutomaton[Char],
    val start : PushdownAutomaton.State) {
  import dk.itu.coqoon.ui.text.PushdownAutomaton
  import scala.collection.mutable.ListBuffer
  private var trace : ListBuffer[(automaton.Execution, Int)] = ListBuffer()

  def update(
      offset : Int, length : Int, content : String, document : String) : Unit =
    update(offset, length, content.length, document.charAt, document.length)

  def update(offset : Int,
      length : Int, content : String, document : IDocument) : Unit =
    update(
        offset, length, content.length, document.getChar, document.getLength)

  def update(offset : Int, length : Int, replacedBy : Int,
      charAt : Int => Char, totalLength : Int) = {
    val oldLength = totalLength - replacedBy + length
    var traceFragment : ListBuffer[(automaton.Execution, Int)] = ListBuffer()

    /* If we already have a view of the executions in the document, then
     * annotate it with their indices and offsets (as a stream to minimise
     * unnecessary computation).
     *
     * The main loop below runs through the automaton with the new document and
     * iterates through this sequence for the old one. The indices are used to
     * support that iteration, and the offsets are used to detect when the new
     * document and the old one resynchronise with each other. */
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
     * at. */
    val beginAt = tfs.collectFirst {
      case q @ (i, e, s, o)
          if offset >= o && offset <= (o + s) =>
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
    var oldState = beginAt
    var oldPosition = Math.min(offset, oldLength)

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
        /* Feed the new document into the recogniser, noting when the execution
         * state changes. (Trivial transitions, like going from a state to
         * itself without modifying the stack, can be safely ignored.) */
        newExec.accept(charAt(position)) match {
          case Some((_, e)) if e == newExec =>
            newExecLength += 1
          case Some((t, e)) =>
            assert(newExecLength != 0,
                s"$newExec has invalid length $newExecLength")
            traceFragment.append((newExec, newExecLength))
            newExec = e
            newExecLength = 1
          case _ =>
            /* The automaton rejected the character at this position, but this
             * should never happen in a tokenisable document */
            ???
        }
        /* Iterate through the execution states of the old document, if there
         * is one, to find out when we're back in sync -- i.e., when the old
         * state and the new state are the same /and/ the position in the two
         * documents is the same (after taking @length and @content into
         * consideration). */
        oldState foreach {
          case o @ (oi, oe, os, ol) =>
            if (oldPosition >= (os + ol)) {
              /* We got to the end of this region; advance to the next one */
              oldState =
                if (tfs.isDefinedAt(oi + 1)) {
                  Some(tfs(oi + 1))
                } else None
            } else if (oe == newExec &&
                (position - length + replacedBy) == oldPosition) {
              /* The old machine and the new one are back in sync: fix up the
               * length of this execution state and stop evaluating things.*/
              val iNEL = newExecLength
              newExecLength += ol - (oldPosition + 1) + os
              assert(newExecLength > 0,
                  s"$newExec has invalid length $newExecLength")
              break
            }
            oldPosition += 1
        }
        position += 1
      }
    }
    if ((position - offset) != 0) {
      /* The old and the new documents didn't immediately sync on the first
       * character, so the document's partitioning has actually changed; update
       * it accordingly. */
      if (newExecLength != 0)
        traceFragment.append((newExec, newExecLength))

      val firstIndex = beginAt.map(_._1).getOrElse(0)
      val lastIndex = oldState.map(_._1 + 1).getOrElse(trace.length)
      this.trace.remove(firstIndex, (lastIndex - firstIndex))
      this.trace.insertAll(firstIndex, traceFragment)
    }
    /* XXX: what would be a useful thing to return here? */
  }

  /* Returns the live sequence of executions and lengths used as the internal
   * document representation. */
  def getExecutions() = trace
}