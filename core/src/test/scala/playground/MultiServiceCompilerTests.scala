package playground

import weaver._
import playground.smithyql.WithSource
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import cats.data.NonEmptyList
import playground.smithyql.UseClause
import weaver.scalacheck.Checkers
import playground.smithyql.Arbitraries._

object MultiServiceCompilerTests extends SimpleIOSuite with Checkers {

  test("resolveService with no use clause and one service") {
    forall {
      (
        op: WithSource[OperationName],
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
        op: WithSource[OperationName],
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

  test("resolveService with one service and a mismatching clause") {
    forall {
      (
        op: WithSource[OperationName],
        useClause: WithSource[UseClause],
        name: String,
      ) =>
        val ident = useClause.value.identifier
        val otherIdent = ident.copy(selection = ident.selection + "Valid")

        val result = MultiServiceCompiler.resolveService(
          Some(useClause),
          op,
          Map(
            otherIdent -> name
          ),
        )
        val expected = CompilationFailed(
          NonEmptyList.one(
            CompilationError(
              CompilationErrorDetails.UnknownService(ident, List(otherIdent)),
              useClause.range,
            )
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
        op: WithSource[OperationName],
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

        val expected = CompilationFailed(
          NonEmptyList.one(
            CompilationError(
              CompilationErrorDetails.AmbiguousService(allServices.keySet.toList),
              op.range,
            )
          )
        )

        assert(result == Left(expected))
    }
  }

}
