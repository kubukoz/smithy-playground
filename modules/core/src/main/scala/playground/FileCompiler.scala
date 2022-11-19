package playground

import cats.Parallel
import cats.effect.implicits._
import cats.implicits._
import cats.~>
import playground._
import playground.smithyql.SourceFile
import playground.smithyql.WithSource
import playground.smithyql.Prelude
import cats.Applicative
import cats.ApplicativeError
import cats.data.NonEmptyList

trait FileCompiler[F[_]] {
  def compile(f: SourceFile[WithSource]): F[List[CompiledInput]]
  def mapK[G[_]](fk: F ~> G): FileCompiler[G] = f => fk(compile(f))
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
            // todo: should this use Eff.perform? EffF.perform?
            // todo: we should seal in this.compile somewhere
            .run(OperationCompiler.Context(f.prelude))

    }

}

trait PreludeCompiler[F[_]] {
  def compile(f: Prelude[WithSource]): F[Unit]
}

object PreludeCompiler {

  def instance[F[_]: Parallel](
    serviceIndex: ServiceIndex
  )(
    implicit F: ApplicativeError[F, NonEmptyList[CompilationError]]
  ): PreludeCompiler[F] =
    new PreludeCompiler[F] {

      def compile(f: Prelude[WithSource]): F[Unit] = f.useClauses.parTraverse_ { clause =>
        val serviceId = clause.value.identifier.value

        serviceIndex.getService(serviceId) match {
          case None =>
            CompilationError
              .error(
                CompilationErrorDetails.UnknownService(serviceIndex.serviceIds.toList),
                clause.value.identifier.range,
              )
              .pure[NonEmptyList]
              .raiseError[F, Unit]

          case Some(_) => Applicative[F].unit
        }

      }

    }

}