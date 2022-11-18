package playground

import cats.Id
import cats.implicits._
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.QueryOperationName
import playground.smithyql.UseClause
import weaver._

object MultiServiceResolverTests extends FunSuite {
  private def mkIndex(servicesToOps: (QualifiedIdentifier, Set[OperationName[Id]])*): ServiceIndex =
    ServiceIndex.fromServiceOperationMappings(servicesToOps.toMap)

  test("no explicit/implicit service ref, no services available") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(),
      useClauses = Nil,
    )

    assert.same(
      result,
      ResolutionFailure.AmbiguousService(Nil, Nil).leftNel,
    )
  }

  test("no explicit/implicit service ref, some services available") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "AvailableA") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "AvailableB") -> Set.empty,
      ),
      useClauses = Nil,
    )

    assert.same(
      result,
      ResolutionFailure
        .AmbiguousService(
          workspaceServices = List(
            QualifiedIdentifier.of("com", "example", "AvailableA"),
            QualifiedIdentifier.of("com", "example", "AvailableB"),
          ),
          matchingServices = Nil,
        )
        .leftNel,
    )
  }

  test("explicit service ref that doesn't match any known service") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = Some(QualifiedIdentifier.of("com", "example", "Unavailable")),
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "AvailableA") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "AvailableB") -> Set.empty,
      ),
      useClauses = Nil,
    )

    assert.same(
      result,
      ResolutionFailure
        .UnknownService(
          QualifiedIdentifier.of("com", "example", "Unavailable"),
          List(
            QualifiedIdentifier.of("com", "example", "AvailableA"),
            QualifiedIdentifier.of("com", "example", "AvailableB"),
          ),
        )
        .leftNel,
    )
  }

  test("explicit service ref that matches a service, but the service doesn't have that operation") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = Some(QualifiedIdentifier.of("com", "example", "ServiceMissingOp")),
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "ServiceA") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "ServiceMissingOp") -> Set(OperationName("Op2")),
      ),
      useClauses = Nil,
    )

    assert.same(
      result,
      ResolutionFailure
        .OperationMissing(
          OperationName("Op"),
          QualifiedIdentifier.of("com", "example", "ServiceMissingOp"),
          Set(OperationName("Op2")),
        )
        .leftNel,
    )
  }

  test("explicit service ref that matches one service") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = Some(QualifiedIdentifier.of("com", "example", "MatchingService")),
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(OperationName("Op")),
      ),
      useClauses = Nil,
    )

    assert.same(
      result,
      QualifiedIdentifier.of("com", "example", "MatchingService").asRight,
    )
  }

  test(
    "explicit service ref matches one service, but there's a use clause that doesn't match any"
  ) {
    def result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = Some(QualifiedIdentifier.of("com", "example", "MatchingService")),
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(OperationName("Op")),
      ),
      useClauses = List(
        UseClause[Id](QualifiedIdentifier.of("com", "example", "OtherService")),
        UseClause[Id](QualifiedIdentifier.of("com", "example", "UnknownService")),
      ),
    )

    assert(Either.catchOnly[IllegalArgumentException](result.void).isLeft)
  }

  test(
    "no explicit service ref, multiple clauses match"
  ) {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "MatchingService1") -> Set(
          OperationName("Op")
        ),
        QualifiedIdentifier.of("com", "example", "MatchingService2") -> Set(
          OperationName("Op"),
          OperationName("Op2"),
        ),
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set(
          OperationName("Op"),
          OperationName("Op3"),
        ),
      ),
      useClauses = List(
        UseClause[Id](QualifiedIdentifier.of("com", "example", "MatchingService1")),
        UseClause[Id](QualifiedIdentifier.of("com", "example", "MatchingService2")),
      ),
    )

    assert.same(
      result,
      ResolutionFailure
        .AmbiguousService(
          workspaceServices = List(
            QualifiedIdentifier.of("com", "example", "MatchingService1"),
            QualifiedIdentifier.of("com", "example", "MatchingService2"),
            QualifiedIdentifier.of("com", "example", "OtherService"),
          ),
          matchingServices = List(
            QualifiedIdentifier.of("com", "example", "MatchingService1"),
            QualifiedIdentifier.of("com", "example", "MatchingService2"),
          ),
        )
        .leftNel,
    )
  }

  test(
    "no explicit service ref, multiple clauses match but they're for the same service (deduplication)"
  ) {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(
          OperationName("Op")
        ),
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set(
          OperationName("Op"),
          OperationName("Op3"),
        ),
      ),
      useClauses = List(
        UseClause[Id](QualifiedIdentifier.of("com", "example", "MatchingService")),
        UseClause[Id](QualifiedIdentifier.of("com", "example", "MatchingService")),
      ),
    )

    assert.same(
      result,
      QualifiedIdentifier.of("com", "example", "MatchingService").asRight,
    )
  }

  test(
    "no explicit service ref, exactly one of the clauses matches"
  ) {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      serviceIndex = mkIndex(
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(
          OperationName("Op"),
          OperationName("Op2"),
        ),
      ),
      useClauses = List(
        UseClause[Id](QualifiedIdentifier.of("com", "example", "MatchingService"))
      ),
    )

    assert.same(
      result,
      QualifiedIdentifier.of("com", "example", "MatchingService").asRight,
    )
  }

}
