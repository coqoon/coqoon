package dk.itu.coqoon.ui.utilities

import dk.itu.coqoon.core.debug.CoqoonDebugPreferences.Profiling

object Profile {
  def apply[A](name : String*)(a : => A) : A = {
    val start = System.nanoTime()
    try {
      a
    } finally {
      val duration = (System.nanoTime() - start).toDouble / 1000000000.0d
      Profiling.log(s"${name.mkString("/")} finished after $duration seconds")
    }
  }
}