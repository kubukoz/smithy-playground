package playground

import weaver._
import playground.smithyql.WithSource
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.UseClause
import weaver.scalacheck.Checkers
import playground.smithyql.Arbitraries._

object MultiServiceResolverTests extends SimpleIOSuite with Checkers {

  test("resolveService with no use clause and one service") {
    forall {
      (
        ident: QualifiedIdentifier,
        name: String,
      ) =>
        val result = MultiServiceResolver.resolveService(
          None,
          Map(
            ident -> name
          ),
        )

        assert(result == Right(name))
    }
  }

  test("resolveService with any amount of services and a matching clause") {
    forall {
      (
        useClause: WithSource[UseClause],
        name: String,
        otherServices: Map[QualifiedIdentifier, String],
      ) =>
        val result = MultiServiceResolver.resolveService(
          Some(useClause),
          otherServices ++ Map(
            useClause.value.identifier -> name
          ),
        )

        assert(result == Right(name))
    }
  }

  test("resolveService with any amount of services and a mismatching clause") {
    forall {
      (
        useClause: WithSource[UseClause],
        services: Map[QualifiedIdentifier, String],
      ) =>
        val ident = useClause.value.identifier

        val result = MultiServiceResolver.resolveService(
          Some(useClause),
          services - useClause.value.identifier,
        )

        val expected = MultiServiceResolver
          .ResolutionFailure
          .UnknownService(ident, services.keySet.toList)

        assert(
          result == Left(
            expected
          )
        )
    }
  }

  test("resolveService with multiple services and no clause") {
    forall {
      (
        services: Map[QualifiedIdentifier, String],
        extraService1: (QualifiedIdentifier, String),
        extraService2: (QualifiedIdentifier, String),
      ) =>
        val allServices = services + extraService1 + extraService2

        val result = MultiServiceResolver.resolveService(
          useClause = None,
          services = allServices,
        )

        val expected = MultiServiceResolver
          .ResolutionFailure
          .AmbiguousService(allServices.keySet.toList)

        assert(result == Left(expected))
    }
  }

}
