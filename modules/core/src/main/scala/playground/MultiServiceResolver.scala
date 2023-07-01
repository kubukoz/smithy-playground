package playground

import cats.data.EitherNel
import cats.implicits._
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.QueryOperationName
import playground.smithyql.UseClause
import playground.smithyql.WithSource

object MultiServiceResolver {

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
    * This method assumes that all of the use clauses match the available service set. It does NOT
    * perform a check on that. For the actual check, see PreludeCompiler.
    */
  def resolveService(
    queryOperationName: QueryOperationName[WithSource],
    serviceIndex: ServiceIndex,
    useClauses: List[UseClause[WithSource]],
  ): EitherNel[CompilationError, QualifiedIdentifier] =
    queryOperationName.identifier match {
      case Some(explicitRef) =>
        resolveExplicit(serviceIndex, explicitRef, queryOperationName.operationName)

      case None => resolveImplicit(queryOperationName.operationName, serviceIndex, useClauses)
    }

  private def resolveExplicit(
    index: ServiceIndex,
    explicitRef: WithSource[QualifiedIdentifier],
    operationName: WithSource[OperationName[WithSource]],
  ): EitherNel[CompilationError, QualifiedIdentifier] =
    index.getService(explicitRef.value) match {
      // explicit reference exists, but the service doesn't
      case None =>
        CompilationError
          .error(
            CompilationErrorDetails.UnknownService(index.serviceIds.toList),
            explicitRef.range,
          )
          .leftNel

      // the service exists, but doesn't have the requested operation
      case Some(service)
          if !service.operationNames.contains_(operationName.value.mapK(WithSource.unwrap)) =>
        CompilationError
          .error(
            CompilationErrorDetails.OperationMissing(service.operationNames.toList),
            operationName.range,
          )
          .leftNel

      // all good
      case Some(_) => explicitRef.value.asRight
    }

  private def resolveImplicit(
    operationName: WithSource[OperationName[WithSource]],
    index: ServiceIndex,
    useClauses: List[UseClause[WithSource]],
  ): EitherNel[CompilationError, QualifiedIdentifier] = {
    val matchingServices = index
      .getServices(useClauses.map(_.identifier).map(_.value).toSet)
      .filter(_.hasOperation(operationName.value.mapK(WithSource.unwrap)))

    matchingServices match {
      case one :: Nil => one.id.asRight
      case _ =>
        CompilationError
          .error(
            CompilationErrorDetails
              .AmbiguousService(
                workspaceServices = index.serviceIds.toList
              ),
            operationName.range,
          )
          .leftNel
    }
  }

}
