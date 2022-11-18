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

  /** Determines which service should be used for a query. The rules are:
    *   - If the operation name has a service identifier, there MUST be a service with that name
    *     that contains the given operation.
    *   - If there's no service identifier, find all matching services that are included in the use
    *     clauses. MUST find exactly one entry.
    *
    * In other cases, such as when we can't find a unique entry, or the explicitly referenced
    * service doesn't have an operation with a matching name, we fail. The latter might eventually
    * be refactored to a separate piece of code. This also validates that the services being used
    * are available.
    */
  def resolveService(
    queryOperationName: QueryOperationName[Id],
    servicesToOps: Map[QualifiedIdentifier, Set[OperationName[Id]]],
    useClauses: List[UseClause[Id]],
  ): EitherNel[ResolutionFailure, QualifiedIdentifier] =
    queryOperationName.identifier match {
      case Some(explicitRef) if !servicesToOps.contains(explicitRef) =>
        ResolutionFailure.UnknownService(explicitRef, servicesToOps.keySet.toList).leftNel

      case Some(explicitRef)
          if !servicesToOps(explicitRef).contains_(queryOperationName.operationName) =>
        ResolutionFailure
          .OperationMissing(
            queryOperationName.operationName,
            explicitRef,
            servicesToOps(explicitRef),
          )
          .leftNel
      case Some(explicitRef) => explicitRef.asRight

      case None => ResolutionFailure.AmbiguousService(servicesToOps.keySet.toList).leftNel
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

  final case class OperationMissing(
    operationName: OperationName[Id],
    serviceName: QualifiedIdentifier,
    availableOperations: Set[OperationName[Id]],
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
