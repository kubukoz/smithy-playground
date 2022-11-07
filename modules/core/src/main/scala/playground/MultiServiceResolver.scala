package playground

import cats.implicits._
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.SourceRange
import playground.smithyql.WithSource
import playground.smithyql.UseClause
import cats.Id
import playground.smithyql.QueryOperationName
import playground.smithyql.OperationName
import cats.data.NonEmptyList
import cats.data.EitherNel

object MultiServiceResolver {

  @deprecated("use overload with 3 parameters")
  def resolveService[A](
    identifiers: List[QualifiedIdentifier],
    services: Map[QualifiedIdentifier, A],
  ): Either[ResolutionFailure, A] = ???

  def resolveService(
    queryOperationName: QueryOperationName[Id],
    servicesToOps: Map[QualifiedIdentifier, Set[OperationName[Id]]],
    useClauses: List[UseClause[Id]],
  ): EitherNel[ResolutionFailure, QualifiedIdentifier] = {

    val invalidUseClauses = useClauses.filterNot()

    val servicesInScope = servicesToOps.view.filterKeys(useClauses.map(_.identifier).toSet).toMap

    queryOperationName.identifier match {
      case None =>
        resolveFromClauses(
          queryOperationName.operationName,
          servicesInScope,
        )
      // case body :: Nil => resolveFromOne(Some(body) /* once told me */, services)
      // case more =>
      //   ResolutionFailure
      //     .ConflictingServiceReference(more)
      //     .asLeft
    }
  }

  private def resolveFromClauses(
    operationName: OperationName[Id],
    servicesToOps: Map[QualifiedIdentifier, Set[OperationName[Id]]],
  ): EitherNel[ResolutionFailure, QualifiedIdentifier] =
    servicesToOps.filter { case (_, ops) => ops.contains_(operationName) }.toList match {
      case (head, _) :: Nil => Right(head)
      case Nil => ResolutionFailure.AmbiguousService(servicesToOps.keySet.toList).leftNel
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
