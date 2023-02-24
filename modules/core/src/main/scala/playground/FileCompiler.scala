package playground

import cats.Parallel
import cats.effect.implicits._
import cats.implicits._
import cats.~>
import playground._
import playground.smithyql.SourceFile
import playground.smithyql.WithSource

trait FileCompiler[F[_]] {

  def compile(
    f: SourceFile[WithSource]
  ): F[List[CompiledInput]]

  def mapK[G[_]](
    fk: F ~> G
  ): FileCompiler[G] = f => fk(compile(f))

}

object FileCompiler {

  def instance[F[_]: Parallel](
    preludeCompiler: PreludeCompiler[F],
    opCompiler: OperationCompiler[OperationCompiler.EffF[F, *]],
  ): FileCompiler[F] =
    new FileCompiler[F] {

      def compile(
        f: SourceFile[WithSource]
      ): F[List[CompiledInput]] =
        preludeCompiler.compile(f.prelude) &>
          f
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
            // https://github.com/kubukoz/smithy-playground/issues/157
            // should this use Eff.perform? EffF.perform?
            // we should seal in this.compile somewhere
            .run(OperationCompiler.Context(f.prelude))

    }

}
