package playground

import cats.data.EitherNel
import cats.effect.implicits._
import cats.implicits._
import playground._
import playground.smithyql.SourceFile
import playground.smithyql.WithSource

object FileRunner {

  // Not sure if this abstraction makes sense
  trait Resolver[F[_]] {

    def get(
      file: SourceFile[WithSource]
    ): EitherNel[OperationRunner.Issue, List[OperationRunner[F]]]

  }

  def instance[F[_]](forOperation: OperationRunner.Resolver[F]): Resolver[F] =
    _.statements
      .map(_.fold(_.query.value))
      // keeping toEither on this level for now:
      // - if we had a Both, we can ignore the errors.
      // - if we had a Left/Right, that'll still be the case
      // hoping that we won't need non-protocol Runner.Issues in the current form once this lands:
      // https://github.com/disneystreaming/smithy4s/issues/501
      .parTraverse(forOperation.get(_).toEither)

}
