package playground

import weaver._
import playground.smithyql.WithSource
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.UseClause
import weaver.scalacheck.Checkers
import playground.smithyql.Arbitraries._

object MultiServiceCompilerTests extends SimpleIOSuite with Checkers {

  test("resolveService with no use clause and one service") {
    forall {
      (
        op: WithSource[Unit],
        ident: QualifiedIdentifier,
        name: String,
      ) =>
        val result = MultiServiceCompiler.resolveService(
          None,
          op,
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
        op: WithSource[Unit],
        useClause: WithSource[UseClause],
        name: String,
        otherServices: Map[QualifiedIdentifier, String],
      ) =>
        val result = MultiServiceCompiler.resolveService(
          Some(useClause),
          op,
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
        op: WithSource[Unit],
        useClause: WithSource[UseClause],
        services: Map[QualifiedIdentifier, String],
      ) =>
        val ident = useClause.value.identifier

        val result = MultiServiceCompiler.resolveService(
          Some(useClause),
          op,
          services - useClause.value.identifier,
        )

        val expected = CompilationFailed.one(
          CompilationError(
            CompilationErrorDetails.UnknownService(ident, services.keySet.toList),
            useClause.range,
          )
        )

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
        op: WithSource[Unit],
        services: Map[QualifiedIdentifier, String],
        extraService1: (QualifiedIdentifier, String),
        extraService2: (QualifiedIdentifier, String),
      ) =>
        val allServices = services + extraService1 + extraService2

        val result = MultiServiceCompiler.resolveService(
          useClause = None,
          op = op,
          services = allServices,
        )

        val expected = CompilationFailed.one(
          CompilationError(
            CompilationErrorDetails.AmbiguousService(allServices.keySet.toList),
            op.range,
          )
        )

        assert(result == Left(expected))
    }
  }

}
