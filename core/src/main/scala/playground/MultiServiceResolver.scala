package playground

import cats.implicits._
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.SourceRange
import playground.smithyql.UseClause
import playground.smithyql.WithSource

object MultiServiceResolver {

  def resolveService[A](
    useClause: Option[WithSource[UseClause]],
    services: Map[QualifiedIdentifier, A],
  ): Either[ResolutionFailure, A] =
    useClause match {
      case None if services.sizeIs == 1 => services.head._2.asRight
      case None => ResolutionFailure.AmbiguousService(services.keySet.toList).asLeft

      case Some(clause) =>
        services
          .get(clause.value.identifier)
          .toRight(
            ResolutionFailure.UnknownService(clause.value.identifier, services.keySet.toList)
          )
    }

}

sealed trait ResolutionFailure extends Product with Serializable

object ResolutionFailure {
  final case class AmbiguousService(knownServices: List[QualifiedIdentifier])
    extends ResolutionFailure

  final case class UnknownService(
    unknownId: QualifiedIdentifier,
    knownServices: List[QualifiedIdentifier],
  ) extends ResolutionFailure

  // Returns the preferred range for diagnostics about resolution failure
  def diagnosticRange(q: Query[WithSource]): SourceRange =
    q.useClause match {
      case None         => q.operationName.range
      case Some(clause) => clause.range
    }

}
