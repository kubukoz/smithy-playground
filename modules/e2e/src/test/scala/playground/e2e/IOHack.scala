package cats.effect

object IOHack {

  def fiberSnapshot(): Unit = {
    val runtime = cats.effect.unsafe.implicits.global
    val snapshot = runtime.fiberMonitor.liveFiberSnapshot()

    snapshot.workers.foreach { case (worker, fibers) =>
      System.err.println(s"Worker: $worker")
      fibers.foreach(fiber => System.err.println(s"  $fiber"))
    }

    System.err.println("External fibers:")
    snapshot.external.foreach(fiber => System.err.println(s"  $fiber"))
  }

}
