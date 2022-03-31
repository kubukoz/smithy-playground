package playground

import cats.implicits._
import cats.parse.Parser.Expectation.InRange
import playground.smithyql.SmithyQLParser
import typings.vscode.mod
import typings.vscode.mod.Diagnostic
import typings.vscode.mod.DiagnosticSeverity
import types._

object highlight {

  def getHighlights[Op[_, _, _, _, _]](
    doc: mod.TextDocument
  )(
    implicit c: Compiler[Op, EitherThrow]
  ): List[Diagnostic] =
    validate.full[Op, EitherThrow](doc.getText()) match {
      case Right(_) => Nil

      case Left(SmithyQLParser.ParsingFailure(e, _)) =>
        val pos = doc.positionAt(e.failedAtOffset.toDouble)
        val range = doc
          .getWordRangeAtPosition(pos)
          .getOrElse(new mod.Range(pos, doc.lineAt(doc.lineCount - 1).range.end))

        List(
          error(
            "Parsing failure: expected one of " + e
              .expected
              .map {
                case InRange(_, lower, upper) if lower == upper => lower.toString
                case InRange(_, lower, upper)                   => s"$lower-$upper"
                case msg                                        => msg.toString()
              }
              .mkString_(", "),
            range,
          )
        )

      case Left(e) =>
        val defaultRange =
          new mod.Range(doc.lineAt(0).range.start, doc.lineAt(doc.lineCount - 1).range.end)

        e match {
          case CompilationFailed(errors) =>
            errors.map { ee => // dźwig
              error(
                ee.render,
                adapters.toVscodeRange(doc, ee.range),
              )
            }.toList

          case _ =>
            List(
              error(
                "Unexpected compilation failure: " + Option(e.getMessage()).getOrElse("null"),
                defaultRange,
              )
            )
        }

    }

  private def error(msg: String, range: mod.Range) =
    new Diagnostic(range, msg, DiagnosticSeverity.Error)

}
