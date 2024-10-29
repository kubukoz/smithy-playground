package playground

import cats.data.NonEmptyList
import cats.syntax.all.*
import playground.smithyql.SourceFile
import playground.smithyql.SourceRange
import playground.smithyql.WithSource

object FileRunner {

  // Not sure if this abstraction makes sense
  trait Resolver[F[_]] {

    def get(
      file: SourceFile[WithSource]
    ): Either[NonEmptyList[
      (
        SourceRange,
        OperationRunner.Issue.Squashed,
      )
    ], List[OperationRunner[F]]]

  }

  def instance[F[_]](
    forOperation: OperationRunner.Resolver[F]
  ): Resolver[F] =
    file =>
      file
        .queries(WithSource.unwrap)
        .map(_.query.value)
        // keeping toEither on this level for now:
        // - if we had a Both, we can ignore the errors.
        // - if we had a Left/Right, that'll still be the case
        // hoping that we won't need non-protocol Runner.Issues in the current form once this lands:
        // https://github.com/disneystreaming/smithy4s/issues/501
        .parTraverse { q =>
          forOperation
            .get(q, file.prelude)
            .toEither
            .leftMap(OperationRunner.Issue.squash(_))
            .leftMap((q.operationName.range, _))
            .toEitherNel
        }

}
