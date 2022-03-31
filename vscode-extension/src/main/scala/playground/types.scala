package playground

import cats.~>
import cats.effect.IO
import cats.implicits._

object types {

  type EitherThrow[+A] = Either[Throwable, A]

  val eitherToIO: EitherThrow ~> IO =
    new (EitherThrow ~> IO) {
      override def apply[A](fa: EitherThrow[A]): IO[A] = fa.liftTo[IO]
    }

}
