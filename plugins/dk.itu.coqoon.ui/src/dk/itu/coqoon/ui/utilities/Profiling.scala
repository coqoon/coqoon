package dk.itu.coqoon.ui.utilities

import dk.itu.coqoon.core.debug.CoqoonDebugPreferences.Profiling

trait Profiler {
  def apply[A](scope : String)(block : => A) : A
}
object Profiler {
  def apply() : Profiler =
    if (Profiling.get()) {
      new DebugProfiler()
    } else NullProfiler
}

class DebugProfiler private[utilities] extends Profiler {
  private var scopes : List[(Long, String)] = List()
  override def apply[A](scope : String)(block : => A) = {
    scopes +:= (System.nanoTime, scope)
    try {
      block
    } finally {
      val (start, _) = scopes.head
      val duration = (System.nanoTime() - start).toDouble / 1000000000.0d
      val name = scopes.map(_._2).reverse.mkString("/")
      Profiling.log(s"$name finished after $duration seconds")
      scopes = scopes.tail
    }
  }
}
object NullProfiler extends Profiler {
  override def apply[A](scope : String)(block : => A) = block
}