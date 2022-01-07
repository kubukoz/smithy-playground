package playground

import cats.~>
import cats.effect.IO
import cats.implicits._
import demo.smithy.DemoServiceOperation
import demo.smithy.DemoServiceGen

object types {

  type Op[I, E, O, S, A] = DemoServiceOperation[I, E, O, S, A]
  type Alg[Opp[_, _, _, _, _]] = DemoServiceGen[Opp]
  type EitherThrow[+A] = Either[Throwable, A]

  val eitherToIO: EitherThrow ~> IO =
    new (EitherThrow ~> IO) {
      override def apply[A](fa: EitherThrow[A]): IO[A] = fa.liftTo[IO]
    }

}
