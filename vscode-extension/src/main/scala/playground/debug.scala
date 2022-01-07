package playground

object debug {

  def timed[A](tag: String)(f: => A): A = {
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    println(s"$tag took ${(end - start) / 1000000}ms")
    result
  }

}
