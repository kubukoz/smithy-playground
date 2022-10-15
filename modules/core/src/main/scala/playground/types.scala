package playground

import cats.data.Ior

object types {

  type IorThrow[+A] = Ior[Throwable, A]

  implicit final class OptionOps[E](private val opt: Option[E]) extends AnyVal {

    def toBothLeft[A](a: A): Ior[E, A] = opt.fold(Ior.right[E, A](a))(Ior.both(_, a))

  }

}
