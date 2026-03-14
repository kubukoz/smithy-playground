package cats.effect

object IOHack {

  def fiberSnapshot() = {
    val runtime = cats.effect.unsafe.implicits.global
    runtime.fiberMonitor.liveFiberSnapshot(System.err.print(_))
  }

}
