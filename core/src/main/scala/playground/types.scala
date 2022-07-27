package playground

import cats.~>
import cats.effect.IO
import cats.implicits._
import cats.data.Ior

object types {

  type IorThrow[+A] = Ior[Throwable, A]

  val iorToIO: IorThrow ~> IO =
    new (IorThrow ~> IO) {

      override def apply[A](
        fa: IorThrow[A]
      ): IO[A] = fa.toEither.liftTo[IO]

    }

}
