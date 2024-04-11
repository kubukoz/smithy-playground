package playground

import cats.data.Ior
import cats.data.Ior.Both
import cats.data.Ior.Left
import cats.data.Ior.Right
import cats.kernel.Semigroup
import cats.syntax.all.*

object IorUtils {

  def orElseCombine[A: Semigroup, B](
    lhs: Ior[A, B],
    rhs: Ior[A, B],
  ): Ior[A, B] =
    lhs match {
      case Both(a, b) =>
        rhs match {
          case Both(a2, _) => Both(a |+| a2, b)
          case Left(a2)    => Both(a |+| a2, b)
          case Right(_)    => Both(a, b)
        }
      case Left(a) =>
        rhs match {
          case Both(a2, b2) => Both(a |+| a2, b2)
          case Left(a2)     => Left(a |+| a2)
          case Right(b2)    => Both(a, b2)
        }
      case Right(b) =>
        rhs match {
          case Both(a2, _) => Both(a2, b)
          case Left(a)     => Both(a, b)
          case Right(_)    => Right(b)
        }
    }

}
