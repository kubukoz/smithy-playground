package playground

import cats.Parallel
import cats.effect.implicits._
import cats.implicits._
import cats.~>
import playground._
import playground.smithyql.SourceFile
import playground.smithyql.WithSource

trait FileCompiler[F[_]] {
  def compile(f: SourceFile[WithSource]): F[List[CompiledInput]]
  def mapK[G[_]](fk: F ~> G): FileCompiler[G] = f => fk(compile(f))
}

object FileCompiler {

  def instance[F[_]: Parallel](
    opCompiler: OperationCompiler[OperationCompiler.EffF[F, *]]
  ): FileCompiler[F] =
    new FileCompiler[F] {

      def compile(
        f: SourceFile[WithSource]
      ): F[List[CompiledInput]] = f
        .statements
        .value
        .parTraverse {
          _.fold(runQuery =>
            opCompiler.compile(
              runQuery
                .query
                .value
            )
          )
        }
        .run(OperationCompiler.Context(f.prelude))

    }

}
