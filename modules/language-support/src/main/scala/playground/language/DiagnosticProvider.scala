package playground.language

import cats.data.Ior
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.implicits._
import playground.CompilationError
import playground.CompilationErrorDetails
import playground.CompilationFailed
import playground.DiagnosticSeverity
import playground.FileCompiler
import playground.FileRunner
import playground.OperationRunner
import playground.smithyql.Position
import playground.smithyql.SourceFile
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
    compiler: FileCompiler[IorThrow],
    fileRunner: FileRunner.Resolver[F],
  ): DiagnosticProvider[F] =
    new DiagnosticProvider[F] {

      def getDiagnostics(
        fileName: String,
        documentText: String,
      ): List[CompilationError] = compilationErrors(documentText).fold(
        _.toList,
        parsed => runnerErrors(parsed),
        (
          errors,
          parsed,
        ) => errors.toList ++ runnerErrors(parsed),
      )

      private def runnerErrors(
        parsed: SourceFile[WithSource]
      ): List[CompilationError] =
        fileRunner.get(parsed) match {
          case Right(_)     => Nil
          case Left(errors) => errors.toList.flatMap(makeRunnerError.tupled)
        }

      private def makeRunnerError(
        pos: SourceRange,
        e: OperationRunner.Issue.Squashed,
      ): List[CompilationError] =
        e match {
          case OperationRunner.Issue.Squashed.ProtocolIssues(supported, found) =>
            List(
              CompilationError.info(
                CompilationErrorDetails.UnsupportedProtocols(supported, found),
                pos,
              )
            )
          case OperationRunner.Issue.Squashed.OtherIssues(es) =>
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

      def compilationErrors(
        documentText: String
      ): IorNel[CompilationError, SourceFile[WithSource]] = {
        val defaultRange = SourceRange(Position.origin, Position(documentText.size))

        val base: Ior[Throwable, SourceFile[WithSource]] = SourceParser[SourceFile]
          .parse(documentText)
          .fold(
            // If parsing fails, fail
            Ior.left(_),
            sf =>
              // If compilation fails, pass the errors but discard success
              compiler.compile(sf).as(sf),
          )

        base
          .leftMap {
            case pf @ ParsingFailure(e, _) =>
              val range = SourceRange(Position(e.failedAtOffset), Position(e.failedAtOffset))

              NonEmptyList.one {
                CompilationError.error(
                  CompilationErrorDetails.ParseError(pf.expectationString(verbose = false)),
                  range,
                )
              }

            case CompilationFailed(errors) => errors

            // get rid of this? hasn't ever been seen and there's no test so it's likely dead code.
            // https://github.com/kubukoz/smithy-playground/issues/162
            case e =>
              NonEmptyList.one {
                error(
                  "Unexpected compilation failure: " + Option(e.getMessage()).getOrElse("null"),
                  defaultRange,
                )
              }

          }
      }

      private def error(
        msg: String,
        range: SourceRange,
      ) = CompilationError.error(CompilationErrorDetails.Message(msg), range)

      private def info(
        msg: String,
        range: SourceRange,
      ) =
        new CompilationError(
          CompilationErrorDetails.Message(msg),
          range,
          DiagnosticSeverity.Information,
          tags = Set.empty,
        )

    }

}
