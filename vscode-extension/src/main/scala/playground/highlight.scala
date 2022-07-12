package playground

import cats.data.IorNel
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.Parser.Expectation.InRange
import playground.smithyql.Query
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import typings.vscode.mod
import typings.vscode.mod.Diagnostic
import typings.vscode.mod.DiagnosticSeverity

import types._
import cats.data.Ior
import playground.CompilationErrorDetails.DuplicateItem
import playground.CompilationErrorDetails.EnumFallback

object highlight {

  def getHighlights[F[_]](
    doc: mod.TextDocument,
    c: Compiler[IorThrow],
    runner: Runner.Optional[F],
  ): List[Diagnostic] = compilationErrors(doc, c).fold(
    _.toList,
    parsed => runnerErrors(doc, parsed, runner),
    (errors, parsed) => errors.toList ++ runnerErrors(doc, parsed, runner),
  )

  def runnerErrors[F[_]](
    doc: mod.TextDocument,
    parsed: Query[WithSource],
    runner: Runner.Optional[F],
  ): List[Diagnostic] =
    runner.get(parsed).toEither match {
      case Left(e) =>
        val pos = adapters.toVscodeRange(doc, parsed.operationName.range)

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

  def compilationErrors[Op[_, _, _, _, _], F[_]](
    doc: mod.TextDocument,
    c: Compiler[IorThrow],
  ): IorNel[Diagnostic, Query[WithSource]] = {

    val base: Ior[Throwable, Query[WithSource]] = SmithyQLParser
      .parseFull(doc.getText())
      .fold(
        // If parsing fails, fail
        Ior.left(_),
        q =>
          // If compilation fails, pass the errors but keep the parsing result
          c.compile(q).as(q),
      )

    base
      .leftMap {
        case SmithyQLParser.ParsingFailure(e, _) =>
          val pos = doc.positionAt(e.failedAtOffset.toDouble)
          val range = new mod.Range(pos, pos.translate(0, 1))

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
          val defaultRange =
            new mod.Range(doc.lineAt(0).range.start, doc.lineAt(doc.lineCount - 1).range.end)

          e match {
            case CompilationFailed(errors) =>
              errors.map { ee => // dźwig

                ee.err match {
                  // todo: support warnings in "CompilationFailed"
                  // todo2: rename CompilationFailed cause it makes no sense with warnings
                  case DuplicateItem | _: EnumFallback =>
                    warn(
                      ee.err.render,
                      adapters.toVscodeRange(doc, ee.range),
                    )
                  case _ =>
                    error(
                      ee.err.render,
                      adapters.toVscodeRange(doc, ee.range),
                    )
                }
              }

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

  private def error(msg: String, range: mod.Range) =
    new Diagnostic(range, msg, DiagnosticSeverity.Error)

  private def warn(msg: String, range: mod.Range) =
    new Diagnostic(range, msg, DiagnosticSeverity.Warning)

  private def info(msg: String, range: mod.Range) =
    new Diagnostic(range, msg, DiagnosticSeverity.Information)

}
