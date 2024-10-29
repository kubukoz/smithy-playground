package playground

import cats.Id
import cats.syntax.all.*
import com.softwaremill.diffx.cats.*
import playground.Diffs.given
import playground.smithyql.Diffs.given
import playground.smithyql.OperationName
import playground.smithyql.Prelude
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.QueryOperationName
import playground.smithyql.SourceRange
import playground.smithyql.StringRangeUtils.*
import playground.smithyql.parser.SourceParser
import weaver.*

import Assertions.*
import ServiceIndex.ServiceMetadata

object MultiServiceResolverTests extends FunSuite {

  private def mkIndex(
    servicesToOps: (
      QualifiedIdentifier,
      Set[OperationName[Id]],
    )*
  ): ServiceIndex = ServiceIndex.fromMappings(
    servicesToOps.map(_.map(ServiceMetadata(_, deprecated = None))).toMap
  )

  private def resolveService(
    operationNameSource: String,
    useClausesSource: String,
    serviceIndex: ServiceIndex,
  ) = {
    val operationName = SourceParser[QueryOperationName].parse(operationNameSource).toTry.get
    val useClauses = SourceParser[Prelude].parse(useClausesSource).toTry.get

    MultiServiceResolver.resolveService(
      queryOperationName = operationName,
      serviceIndex = serviceIndex,
      useClauses = useClauses.useClauses.map(_.value),
    )
  }

  import CompilationError.error

  test("no explicit/implicit service ref, no services available") {
    val result = resolveService(
      "Op",
      "",
      mkIndex(),
    )

    assertNoDiff(
      result,
      error(
        CompilationErrorDetails.AmbiguousService(Nil),
        SourceRange.forEntireString("Op"),
      ).leftNel,
    )
  }

  test("no explicit/implicit service ref, some services available") {
    val result = resolveService(
      "Op",
      "",
      mkIndex(
        QualifiedIdentifier.of("com", "example", "AvailableA") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "AvailableB") -> Set.empty,
      ),
    )

    assertNoDiff(
      result,
      error(
        CompilationErrorDetails
          .AmbiguousService(
            workspaceServices = List(
              QualifiedIdentifier.of("com", "example", "AvailableA"),
              QualifiedIdentifier.of("com", "example", "AvailableB"),
            )
          ),
        SourceRange.forEntireString("Op"),
      ).leftNel,
    )
  }

  test("explicit service ref that doesn't match any known service") {
    val result = resolveService(
      "com.example#Unavailable.Op",
      "",
      mkIndex(
        QualifiedIdentifier.of("com", "example", "AvailableA") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "AvailableB") -> Set.empty,
      ),
    )

    assertNoDiff(
      result,
      error(
        CompilationErrorDetails
          .UnknownService(
            List(
              QualifiedIdentifier.of("com", "example", "AvailableA"),
              QualifiedIdentifier.of("com", "example", "AvailableB"),
            )
          ),
        SourceRange.forEntireString("com.example#Unavailable"),
      ).leftNel,
    )
  }

  test("explicit service ref that matches a service, but the service doesn't have that operation") {
    val result = resolveService(
      "com.example#ServiceMissingOp.TheOp",
      "",
      mkIndex(
        QualifiedIdentifier.of("com", "example", "ServiceA") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "ServiceMissingOp") -> Set(OperationName("Op2")),
      ),
    )

    assertNoDiff(
      result,
      error(
        CompilationErrorDetails
          .OperationMissing(
            List(OperationName("Op2"))
          ),
        "com.example#ServiceMissingOp.TheOp".rangeOf("TheOp"),
      ).leftNel,
    )
  }

  test("explicit service ref that matches one service") {
    val result = resolveService(
      "com.example#MatchingService.Op",
      "",
      mkIndex(
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(OperationName("Op")),
      ),
    )

    assertNoDiff(
      result,
      QualifiedIdentifier.of("com", "example", "MatchingService").asRight,
    )
  }

  test(
    "missing use clause services are ignored at this scope"
  ) {
    val result = resolveService(
      "com.example#MatchingService.Op",
      "use service com.example#UnknownService",
      mkIndex(
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(OperationName("Op"))
      ),
    )

    assert(result.isRight)
  }

  test(
    "no explicit service ref, multiple clauses match"
  ) {
    val result = resolveService(
      "Op",
      """use service com.example#MatchingService1
        |use service com.example#MatchingService2""".stripMargin,
      mkIndex(
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
    )

    assertNoDiff(
      result,
      error(
        CompilationErrorDetails
          .AmbiguousService(
            workspaceServices = List(
              QualifiedIdentifier.of("com", "example", "MatchingService1"),
              QualifiedIdentifier.of("com", "example", "MatchingService2"),
              QualifiedIdentifier.of("com", "example", "OtherService"),
            )
          ),
        SourceRange.forEntireString("Op"),
      ).leftNel,
    )
  }

  test(
    "no explicit service ref, multiple clauses match but they're for the same service (deduplication)"
  ) {
    val result = resolveService(
      "Op",
      """use service com.example#MatchingService
        |use service com.example#MatchingService""".stripMargin,
      mkIndex(
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(
          OperationName("Op")
        ),
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set(
          OperationName("Op"),
          OperationName("Op3"),
        ),
      ),
    )

    assertNoDiff(
      result,
      QualifiedIdentifier.of("com", "example", "MatchingService").asRight,
    )
  }

  test(
    "no explicit service ref, exactly one of the clauses matches"
  ) {
    val result = resolveService(
      "Op",
      "use service com.example#MatchingService",
      mkIndex(
        QualifiedIdentifier.of("com", "example", "OtherService") -> Set.empty,
        QualifiedIdentifier.of("com", "example", "MatchingService") -> Set(
          OperationName("Op"),
          OperationName("Op2"),
        ),
      ),
    )

    assertNoDiff(
      result,
      QualifiedIdentifier.of("com", "example", "MatchingService").asRight,
    )
  }

}
