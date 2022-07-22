package playground

import cats.effect.kernel.GenTemporal
import cats.effect.implicits._
import cats.implicits._
import cats.effect.std

object debug {

  trait UnsafeLog {
    def unsafeLog(s: String): Unit
  }

  def timed[A](tag: String)(f: => A)(implicit unsafeLog: UnsafeLog): A = {
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    unsafeLog.unsafeLog(s"$tag took ${(end - start) / 1000000}ms")
    result
  }

  def timedF[F[_]: std.Console, A](
    tag: String
  )(
    f: F[A]
  )(
    implicit F: GenTemporal[F, _]
  ): F[A] = f.timed.flatMap { case (time, a) =>
    std.Console[F].println(s"$tag took ${time.toMillis}ms").as(a)
  }

}
