package playground

import cats.Id
import cats.implicits._
import playground.smithyql.OperationName
import playground.smithyql.QueryOperationName
import weaver._
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.UseClause
import cats.data.NonEmptyList

object MultiServiceResolverTests extends FunSuite {
  test("no explicit/implicit service ref, no services available") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      servicesToOps = Map.empty,
      useClauses = Nil,
    )

    assert.same(
      result,
      ResolutionFailure.AmbiguousService(Nil).leftNel,
    )
  }

  test("no explicit/implicit service ref, some services available") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      servicesToOps = Map(
        QualifiedIdentifier.of("com", "example", "AvailableA") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "AvailableB") -> Set.empty,
      ),
      useClauses = Nil,
    )

    assert.same(
      result,
      ResolutionFailure
        .AmbiguousService(
          List(
            QualifiedIdentifier.of("com", "example", "AvailableA"),
            QualifiedIdentifier.of("com", "example", "AvailableB"),
          )
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
      servicesToOps = Map(
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
      servicesToOps = Map(
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
      servicesToOps = Map(
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

  /*
  test("use clauses referring to nonexistent services") {
    val result = MultiServiceResolver.resolveService(
      queryOperationName = QueryOperationName[Id](
        identifier = None,
        operationName = OperationName("Op"),
      ),
      servicesToOps = Map(
        QualifiedIdentifier.of("com", "example", "ServiceA") -> Set.empty
      ),
      useClauses = List(
        UseClause(QualifiedIdentifier.of("com", "example", "ServiceA")),
        UseClause(QualifiedIdentifier.of("com", "example", "ServiceB")),
        UseClause(QualifiedIdentifier.of("com", "example", "ServiceC")),
      ),
    )

    val knownServices = List(
      QualifiedIdentifier.of("com", "example", "ServiceA")
    )
    assert.same(
      result,
      NonEmptyList
        .of(
          ResolutionFailure.AmbiguousService(knownServices),
          ResolutionFailure.UnknownService(
            QualifiedIdentifier.of("com", "example", "ServiceB"),
            knownServices,
          ),
          ResolutionFailure.UnknownService(
            QualifiedIdentifier.of("com", "example", "ServiceC"),
            knownServices,
          ),
        )
        .asLeft,
    )
  } */
}
