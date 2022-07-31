package cats.tagless

import cats.FlatMap
import cats.data.Nested
import cats.implicits._
import cats.~>

trait SuspendK[Alg[_[_]]] extends FunctorK[Alg] {
  def suspend[F[_]]: Alg[Suspended[Alg, F, *]]

  def suspendK[F[_], G[_]](fk: Suspended[Alg, F, *] ~> G): Alg[G] =
    mapK[Suspended[Alg, F, *], G](suspend)(fk)

  def deferK[F[_]: FlatMap, G[_]](fa: F[Alg[G]]): Alg[Nested[F, G, *]] = suspendK {
    new (Suspended[Alg, G, *] ~> Nested[F, G, *]) {
      def apply[A](fk: Suspended[Alg, G, A]): Nested[F, G, A] = fa.map(fk.effect(_)).nested
    }
  }

  def deferKId[F[_]: FlatMap](fa: F[Alg[F]]): Alg[F] = suspendK {
    new (Suspended[Alg, F, *] ~> F) {
      def apply[A](fk: Suspended[Alg, F, A]): F[A] = fa.flatMap(fk.effect(_))
    }
  }

}

object SuspendK {
  def apply[Alg[_[_]]](implicit F: SuspendK[Alg]): SuspendK[Alg] = F
}

case class Suspended[Alg[_[_]], F[_], A](effect: Alg[F] => F[A])
