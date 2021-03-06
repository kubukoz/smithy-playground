package playground

import playground.smithyql.Arbitraries._
import playground.smithyql.QualifiedIdentifier
import weaver._
import weaver.scalacheck.Checkers

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
        useClauseIdent: QualifiedIdentifier,
        name: String,
        otherServices: Map[QualifiedIdentifier, String],
      ) =>
        val result = MultiServiceResolver.resolveService(
          Some(useClauseIdent),
          otherServices ++ Map(
            useClauseIdent -> name
          ),
        )

        assert(result == Right(name))
    }
  }

  test("resolveService with any amount of services and a mismatching clause") {
    forall {
      (
        useClauseIdent: QualifiedIdentifier,
        services: Map[QualifiedIdentifier, String],
      ) =>
        val ident = useClauseIdent

        val result = MultiServiceResolver.resolveService(
          Some(useClauseIdent),
          services - useClauseIdent,
        )

        val expected = ResolutionFailure.UnknownService(ident, services.keySet.toList)

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
          useClauseIdentifier = None,
          services = allServices,
        )

        val expected = ResolutionFailure.AmbiguousService(allServices.keySet.toList)

        assert(result == Left(expected))
    }
  }

}
