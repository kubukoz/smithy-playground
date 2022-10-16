package playground.language

import cats.data.Ior
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.Parser.Expectation.InRange
import playground.CompilationError
import playground.CompilationErrorDetails
import playground.CompilationFailed
import playground.Compiler
import playground.DiagnosticSeverity
import playground.Runner
import playground.smithyql.Position
import playground.smithyql.Query
import playground.smithyql.SourceRange
import playground.smithyql.WithSource
import playground.smithyql.parser.ParsingFailure
import playground.smithyql.parser.SourceParser
import playground.types._

trait DiagnosticProvider[F[_]] {

  def getDiagnostics(
    fileName: String,
    documentText: String,
  ): List[CompilationError]

}

object DiagnosticProvider {

  def instance[F[_]](
    compiler: Compiler[IorThrow],
    runner: Runner.Resolver[F],
  ): DiagnosticProvider[F] =
    new DiagnosticProvider[F] {

      def getDiagnostics(fileName: String, documentText: String): List[CompilationError] =
        if (fileName.startsWith("extension-output"))
          // Not showing any diagnostics in output panels
          Nil
        else
          compilationErrors(documentText).fold(
            _.toList,
            parsed => runnerErrors(parsed),
            (errors, parsed) => errors.toList ++ runnerErrors(parsed),
          )

      private def runnerErrors(
        parsed: Query[WithSource]
      ): List[CompilationError] =
        runner.get(parsed).toEither match {
          case Left(e) =>
            val pos = parsed.operationName.range

            Runner.Issue.squash(e) match {
              case Left(ps) =>
                List(
                  info(
                    s"""Service doesn't support any of the available protocols: ${ps
                        .supported
                        .map(_.show)
                        .mkString_(", ")}.
                   |Found protocols: ${ps.found.map(_.show).mkString(", ")}
                   |Running queries will not be possible.""".stripMargin,
                    pos,
                  )
                )
              case Right(es) =>
                es.toList.flatMap {
                  case CompilationFailed(_) => Nil // ignoring to avoid duplicating compiler errors
                  case e =>
                    List(
                      info(
                        s"""Service unsupported. Running queries will not be possible.
                       |Details: $e""".stripMargin,
                        pos,
                      )
                    )
                }
            }
          case Right(_) => Nil
        }

      def compilationErrors(
        documentText: String
      ): IorNel[CompilationError, Query[WithSource]] = {
        val defaultRange = SourceRange(Position.origin, Position(documentText.size))

        val base: Ior[Throwable, Query[WithSource]] = SourceParser[Query]
          .parse(documentText)
          .fold(
            // If parsing fails, fail
            Ior.left(_),
            q =>
              // If compilation fails, pass the errors but keep the parsing result
              compiler.compile(q).as(q),
          )

        base
          .leftMap {
            case ParsingFailure(e, _) =>
              val range = SourceRange(Position(e.failedAtOffset), Position(e.failedAtOffset))

              val oneOfInfix =
                if (e.expected.size > 1)
                  "one of "
                else
                  ""

              NonEmptyList.one {
                error(
                  s"Parsing failure: expected $oneOfInfix" + e
                    .expected
                    .map {
                      case InRange(_, lower, upper) if lower == upper => lower.toString
                      case InRange(_, lower, upper)                   => s"one of $lower-$upper"
                      case msg                                        => msg.toString()
                    }
                    .mkString_(", "),
                  range,
                )
              }

            case e =>
              e match {
                case CompilationFailed(errors) => errors

                case _ =>
                  NonEmptyList.one {
                    error(
                      "Unexpected compilation failure: " + Option(e.getMessage()).getOrElse("null"),
                      defaultRange,
                    )
                  }
              }

          }
      }

      private def error(
        msg: String,
        range: SourceRange,
      ) = CompilationError.error(CompilationErrorDetails.Message(msg), range)

      private def info(msg: String, range: SourceRange) =
        new CompilationError(
          CompilationErrorDetails.Message(msg),
          range,
          DiagnosticSeverity.Information,
          tags = Set.empty,
          relatedInfo = Nil,
        )

    }

}
