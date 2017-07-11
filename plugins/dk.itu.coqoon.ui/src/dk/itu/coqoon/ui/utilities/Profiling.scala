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

object ProfilingProxy {
  import java.lang.reflect.{Proxy, Method, InvocationHandler}
  lazy val cl = getClass.getClassLoader
  def apply[T](base : AnyRef, methodNames : String*) = {
    val handler = new InvocationHandler {
      override def invoke(
          proxy : AnyRef, method : Method, args : Array[AnyRef]) = {
        if (methodNames.length == 0 || methodNames.contains(method.getName)) {
          val as =
            if (args != null) {
              s"${args.map(_ts).mkString(", ")}"
            } else "null"
          Profiler()(s"${_ts(base)}.${method.getName}($as)") {
            method.invoke(base, args : _*)
          }
        } else method.invoke(base, args : _*)
      }
    }
    def getInterfaces(soFar : Set[Class[_]], layer : Class[_]) : Set[Class[_]] =
      soFar ++ layer.getInterfaces ++
          (Option(layer.getSuperclass).toSeq.flatMap(
              sup => getInterfaces(soFar, sup)))
    val allInterfaces = getInterfaces(Set(), base.getClass)
    Proxy.newProxyInstance(cl, allInterfaces.toArray, handler).asInstanceOf[T]
  }

  private def _ts(a : AnyRef) =
    s"${a.getClass.getSimpleName}@" +
        s"${Integer.toHexString(System.identityHashCode(a))}"
}