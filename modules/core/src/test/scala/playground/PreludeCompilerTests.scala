package playground

import cats.Id
import cats.data.Ior
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.implicits._
import com.softwaremill.diffx.cats._
import playground.smithyql.OperationName
import playground.smithyql.Prelude
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.StringRangeUtils._
import playground.smithyql.parser.SourceParser
import weaver._

import Assertions._
import Diffs._

object PreludeCompilerTests extends FunSuite {

  private def compile(services: ServiceIndex, preludeString: String) = PreludeCompiler
    .instance[IorNel[CompilationError, *]](services)
    .compile(SourceParser[Prelude].parse(preludeString).toTry.get)

  private def metadata(
    operationNames: Set[OperationName[Id]] = Set.empty,
    deprecated: Option[DeprecatedInfo] = None,
  ): ServiceIndex.ServiceMetadata = ServiceIndex.ServiceMetadata(operationNames, deprecated)

  test("Prelude compiler with no services validates empty prelude") {
    val result = compile(
      ServiceIndex.empty,
      "",
    )

    assertNoDiff(
      result,
      ().rightIor,
    )
  }

  test("Prelude compiler with no services fails non-empty prelude") {
    val input = "use service example#ServiceA"
    val result = compile(
      ServiceIndex.empty,
      input,
    )

    assertNoDiff(
      result,
      CompilationError
        .error(
          CompilationErrorDetails.UnknownService(Nil),
          input.rangeOf("example#ServiceA"),
        )
        .leftIor
        .toIorNel,
    )
  }

  test("Prelude compiler with some services fails for the missing ones") {
    val input =
      """use service example#Present1
        |use service example#Missing1
        |use service example#Present2
        |use service example#Missing2""".stripMargin

    val result = compile(
      ServiceIndex.fromMappings(
        Map(
          QualifiedIdentifier.of("example", "Present1") -> metadata(),
          QualifiedIdentifier.of("example", "Present2") -> metadata(),
        )
      ),
      input,
    )

    assertNoDiff(
      result,
      NonEmptyList
        .of(
          CompilationError
            .error(
              CompilationErrorDetails
                .UnknownService(
                  List(
                    QualifiedIdentifier.of("example", "Present1"),
                    QualifiedIdentifier.of("example", "Present2"),
                  )
                ),
              input.rangeOf("example#Missing1"),
            ),
          CompilationError
            .error(
              CompilationErrorDetails
                .UnknownService(
                  List(
                    QualifiedIdentifier.of("example", "Present1"),
                    QualifiedIdentifier.of("example", "Present2"),
                  )
                ),
              input.rangeOf("example#Missing2"),
            ),
        )
        .leftIor,
    )
  }

  test("Prelude compiler highlights deprecated services as such") {
    val input =
      """use service example#Valid1
        |use service example#Deprecated
        |use service example#Valid2""".stripMargin

    val result = compile(
      ServiceIndex.fromMappings(
        Map(
          QualifiedIdentifier.of("example", "Valid1") -> metadata(),
          QualifiedIdentifier.of("example", "Valid2") -> metadata(),
          QualifiedIdentifier.of("example", "Deprecated") ->
            metadata(deprecated = Some(DeprecatedInfo(None, None))),
        )
      ),
      input,
    )

    assertNoDiff(
      result,
      Ior.bothNel(
        CompilationError.deprecation(
          DeprecatedInfo(None, None),
          input.rangeOf("example#Deprecated"),
        ),
        (),
      ),
    )
  }
}
