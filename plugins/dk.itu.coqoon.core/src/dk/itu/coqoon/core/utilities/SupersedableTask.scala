package dk.itu.coqoon.core.utilities

class SupersedableTask(delay : Long) {
  private val lock = new Object

  import java.util.TimerTask

  private var last : Option[TimerTask] = None

  def schedule(f : => Unit) : Unit = lock synchronized {
    last.map(_.cancel)
    last = Some(new TimerTask() {
      override def run =
        try {
          f
        } catch {
          case e : Exception =>
            e.printStackTrace
        }
    })
    last.map(SupersedableTask.timer.schedule(_, delay))
  }
}
object SupersedableTask {
  private val lock = new Object

  import java.util.Timer
  private val timer = new Timer()

  def purge() : Unit = timer.purge()
}

abstract class BatchCollector[A](delay : Int = BatchCollector.DEFAULT_DELAY) {
  private val collectTask = new SupersedableTask(delay)
  private object CollectionLock {
    var items : List[A] = List()
  }
  def add(item : A) =
    CollectionLock synchronized {
      CollectionLock.items :+= item
      collectTask schedule {
        val items =
          CollectionLock synchronized {
            try {
              CollectionLock.items
            } finally CollectionLock.items = List()
          }
        process(items)
      }
    }

  protected def process(items : List[A])
}
object BatchCollector {
  final val DEFAULT_DELAY = 400
}
