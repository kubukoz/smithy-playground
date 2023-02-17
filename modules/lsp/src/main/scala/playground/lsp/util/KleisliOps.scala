package playground.lsp.util

import cats.FlatMap
import cats.data.Kleisli
import cats.implicits._
import cats.~>

object KleisliOps {

  def applyEffectK[F[_]: FlatMap, E](fe: F[E]): Kleisli[F, E, *] ~> F =
    new (Kleisli[F, E, *] ~> F) {
      def apply[A](ra: Kleisli[F, E, A]): F[A] = fe.flatMap(ra.run)
    }

}
