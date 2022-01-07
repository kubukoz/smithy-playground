package playground

import cats.implicits._

import types._
import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.smithyql.SmithyQLParser
import cats.MonadThrow

object validate {

  def full[F[_]: MonadThrow](
    q: String
  )(
    implicit compiler: Compiler[Op, F]
  ): F[(Query[WithSource], CompiledInput[Op])] = SmithyQLParser
    .parseFull(q)
    .liftTo[F]
    .mproduct(compiler.compile(_))

}
