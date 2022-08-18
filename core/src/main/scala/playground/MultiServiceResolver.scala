package playground

import cats.implicits._
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.SourceRange
import playground.smithyql.WithSource

object MultiServiceResolver {

  def resolveService[A](
    identifiers: List[QualifiedIdentifier],
    services: Map[QualifiedIdentifier, A],
  ): Either[ResolutionFailure, A] =
    identifiers match {
      case Nil         => resolveFromOne(None, services)
      case body :: Nil => resolveFromOne(Some(body) /* once told me */, services)
      case more =>
        ResolutionFailure
          .ConflictingServiceReference(more)
          .asLeft
    }

  private def resolveFromOne[A](
    ident: Option[QualifiedIdentifier],
    services: Map[QualifiedIdentifier, A],
  ): Either[ResolutionFailure, A] =
    ident match {
      case None if services.sizeIs == 1 => services.head._2.asRight
      case None => ResolutionFailure.AmbiguousService(services.keySet.toList).asLeft

      case Some(ident) =>
        services
          .get(ident)
          .toRight(
            ResolutionFailure.UnknownService(ident, services.keySet.toList)
          )
    }

}

sealed trait ResolutionFailure extends Product with Serializable

object ResolutionFailure {
  final case class AmbiguousService(knownServices: List[QualifiedIdentifier])
    extends ResolutionFailure

  final case class ConflictingServiceReference(references: List[QualifiedIdentifier])
    extends ResolutionFailure

  final case class UnknownService(
    unknownId: QualifiedIdentifier,
    knownServices: List[QualifiedIdentifier],
  ) extends ResolutionFailure

  def toCompilationError(rf: ResolutionFailure, q: Query[WithSource]): CompilationError = {
    val err = CompilationErrorDetails.fromResolutionFailure(rf)

    CompilationError
      .error(
        err,
        defaultRange(q),
      )
      .copy(relatedInfo =
        q.operationName
          .value
          .identifier
          .map { qsr =>
            DiagnosticRelatedInformation(
              RelativeLocation(
                DocumentReference.SameFile,
                qsr.range,
              ),
              err,
            )
          }
          .toList
      )
  }

  // Returns the preferred range for diagnostics about resolution failure
  private def defaultRange(q: Query[WithSource]): SourceRange =
    q.useClause.value match {
      case None         => q.operationName.value.operationName.range
      case Some(clause) => clause.identifier.range
      // todo: involve the optional range in q.operationName's service reference
    }

}
