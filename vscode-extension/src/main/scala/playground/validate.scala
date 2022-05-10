package playground

import cats.implicits._

import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.smithyql.SmithyQLParser
import cats.MonadThrow

object validate {

  def full[F[_]: MonadThrow](
    q: String,
    compiler: Compiler[F],
  ): F[(Query[WithSource], CompiledInput)] = SmithyQLParser
    .parseFull(q)
    .liftTo[F]
    .mproduct(compiler.compile(_))

}
