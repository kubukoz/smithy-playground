package playground

import cats.implicits._
import cats.parse.Parser.Expectation.InRange
import playground.smithyql.SmithyQLParser
import typings.vscode.mod
import typings.vscode.mod.Diagnostic
import typings.vscode.mod.DiagnosticSeverity
import types._
import playground.Runner.Issue.InvalidProtocols
import playground.Runner.Issue.Other
import cats.data.EitherNel
import playground.smithyql.Query
import playground.smithyql.WithSource

object highlight {

  def getHighlights[F[_]](
    doc: mod.TextDocument
  )(
    implicit c: Compiler[EitherThrow],
    runner: Runner.Optional[F],
  ): List[Diagnostic] =
    compilationErrors(doc) match {
      // Even if the file doesn't parse, we want to show runner diagnostics at a default range
      case Left(errors)  => errors.toList ++ runnerErrors(doc, None)
      case Right(parsed) => runnerErrors(doc, parsed.some)
    }

  def runnerErrors[F[_]](
    doc: mod.TextDocument,
    parsed: Option[Query[WithSource]],
  )(
    implicit
    runner: Runner.Optional[F]
  ): List[Diagnostic] =
    runner.get match {
      case Left(e) =>
        val beginningOfDocument = doc
          .getWordRangeAtPosition(doc.positionAt(0d))
          .getOrElse(new mod.Range(0, 0, 0, 1))

        val pos = parsed
          .map(_.operationName.range)
          .map(adapters.toVscodeRange(doc, _))
          .getOrElse(beginningOfDocument)

        e match {
          case InvalidProtocols(ps) =>
            List(
              info(
                s"""Service doesn't support any of the available protocols: ${ps
                  .map(_.show)
                  .mkString_(", ")}.
                   |Running queries will not be possible.""".stripMargin,
                pos,
              )
            )
          case Other(e) =>
            List(
              info(
                s"""Service unsupported. Running queries will not be possible.
                   |Details: $e""".stripMargin,
                pos,
              )
            )
        }
      case Right(_) => Nil
    }

  def compilationErrors[Op[_, _, _, _, _], F[_]](
    doc: mod.TextDocument
  )(
    implicit c: Compiler[EitherThrow]
  ): EitherNel[Diagnostic, Query[WithSource]] =
    validate.full[EitherThrow](doc.getText()) match {
      case Right((parsed, _)) => parsed.asRight

      case Left(SmithyQLParser.ParsingFailure(e, _)) =>
        val pos = doc.positionAt(e.failedAtOffset.toDouble)
        val range = new mod.Range(pos, pos.translate(0, 1))

        val oneOfInfix =
          if (e.expected.size > 1)
            "one of "
          else
            ""

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
        ).leftNel

      case Left(e) =>
        val defaultRange =
          new mod.Range(doc.lineAt(0).range.start, doc.lineAt(doc.lineCount - 1).range.end)

        e match {
          case CompilationFailed(errors) =>
            errors.map { ee => // dźwig
              error(
                ee.err.render,
                adapters.toVscodeRange(doc, ee.range),
              )
            }.asLeft

          case _ =>
            error(
              "Unexpected compilation failure: " + Option(e.getMessage()).getOrElse("null"),
              defaultRange,
            ).leftNel
        }

    }

  private def error(msg: String, range: mod.Range) =
    new Diagnostic(range, msg, DiagnosticSeverity.Error)

  private def info(msg: String, range: mod.Range) =
    new Diagnostic(range, msg, DiagnosticSeverity.Information)

}
