package playground

import cats.implicits._

import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.smithyql.SmithyQLParser
import cats.MonadThrow

object validate {

  def full[Op[_, _, _, _, _], F[_]: MonadThrow](
    q: String
  )(
    implicit compiler: Compiler[Op, F]
  ): F[(Query[WithSource], CompiledInput[Op])] = SmithyQLParser
    .parseFull(q)
    .liftTo[F]
    .mproduct(compiler.compile(_))

}
