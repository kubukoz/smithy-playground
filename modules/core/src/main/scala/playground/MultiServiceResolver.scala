package playground

import cats.Id
import cats.data.EitherNel
import cats.implicits._
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.QueryOperationName
import playground.smithyql.SourceRange
import playground.smithyql.UseClause
import playground.smithyql.WithSource
import cats.data.NonEmptyList

object MultiServiceResolver {

  @deprecated("use overload with 3 parameters")
  def resolveService[A](
    identifiers: List[QualifiedIdentifier],
    services: Map[QualifiedIdentifier, A],
  ): Either[ResolutionFailure, A] = sys.error("resolveService: deprecated, don't use!")

  /** Determines which service should be used for a query. The rules are:
    *   - If the operation name has a service identifier, there MUST be a service with that name
    *     that contains the given operation.
    *   - If there's no service identifier, find all matching services that are included in the use
    *     clauses. MUST find exactly one entry.
    *
    * In other cases, such as when we can't find a unique entry, or the explicitly referenced
    * service doesn't have an operation with a matching name, we fail. The latter might eventually
    * be refactored to a separate piece of code.
    *
    * **Important**!
    *
    * This method assumes and asserts (throwingly) that all of the use clauses match the available
    * service set.
    */
  def resolveService(
    queryOperationName: QueryOperationName[Id],
    serviceIndex: ServiceIndex,
    useClauses: List[UseClause[Id]],
  ): EitherNel[ResolutionFailure, QualifiedIdentifier] = {
    // TODO: checkUseClauses should be executed somewhere up the stack to ensure that even a file without ops doesn't allow these
    require(
      checkUseClauses(useClauses, serviceIndex.serviceIds).isRight,
      "Found use clauses that didn't match any of the available services. This is a bug in smithy-playground!",
    )

    queryOperationName.identifier match {
      case Some(explicitRef) =>
        resolveExplicit(serviceIndex, explicitRef, queryOperationName.operationName)

      case None => resolveImplicit(queryOperationName.operationName, serviceIndex, useClauses)
    }
  }

  private def checkUseClauses(
    clauses: List[UseClause[Id]],
    availableServices: Set[QualifiedIdentifier],
  ): EitherNel[ResolutionFailure, Unit] = clauses.parTraverse_ { clause =>
    availableServices
      .contains_(clause.identifier)
      .guard[Option]
      .toRightNel(ResolutionFailure.UnknownService(clause.identifier, availableServices.toList))
  }

  private def resolveExplicit(
    index: ServiceIndex,
    explicitRef: QualifiedIdentifier,
    operationName: OperationName[Id],
  ): EitherNel[ResolutionFailure, QualifiedIdentifier] =
    index.getService(explicitRef) match {
      // explicit reference exists, but the service doesn't
      case None => ResolutionFailure.UnknownService(explicitRef, index.serviceIds.toList).leftNel

      // the service exists, but doesn't have the requested operation
      case Some(service) if !service.operationNames.contains_(operationName) =>
        ResolutionFailure
          .OperationMissing(
            operationName,
            explicitRef,
            service.operationNames,
          )
          .leftNel

      // all good
      case Some(_) => explicitRef.asRight
    }

  private def resolveImplicit(
    operationName: OperationName[Id],
    index: ServiceIndex,
    useClauses: List[UseClause[Id]],
  ): EitherNel[ResolutionFailure, QualifiedIdentifier] = {
    val matchingServices =
      index
        .requireServices(useClauses.map(_.identifier))
        .filter(_.hasOperation(operationName))
        .toList

    matchingServices match {
      case one :: Nil => one.id.asRight
      case _ =>
        ResolutionFailure
          .AmbiguousService(
            workspaceServices = index.serviceIds.toList,
            matchingServices = matchingServices.map(_.id),
          )
          .leftNel
    }
  }

}

sealed trait ResolutionFailure extends Product with Serializable

object ResolutionFailure {

  final case class AmbiguousService(
    workspaceServices: List[QualifiedIdentifier],
    // services imported with a use clause that are also matching the operation name
    matchingServices: List[QualifiedIdentifier],
  ) extends ResolutionFailure

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
