package playground

import cats.data.IorNel
import cats.data.NonEmptyList
import cats.implicits._
import com.softwaremill.diffx.cats._
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
      ServiceIndex.fromServiceOperationMappings(
        Map(
          QualifiedIdentifier.of("example", "Present1") -> Set.empty,
          QualifiedIdentifier.of("example", "Present2") -> Set.empty,
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
}
