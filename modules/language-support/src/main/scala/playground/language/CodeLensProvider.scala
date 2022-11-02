package playground.language

import cats.implicits._
import playground.FileCompiler
import playground.FileRunner
import playground.smithyql.SourceFile
import playground.smithyql.SourceRange
import playground.smithyql.parser.SourceParser
import playground.types._

trait CodeLensProvider[F[_]] {
  def provide(documentUri: Uri, documentText: String): List[CodeLens]
}

object CodeLensProvider {

  def instance[F[_]](
    compiler: FileCompiler[IorThrow],
    runner: FileRunner.Resolver[F],
  ): CodeLensProvider[F] =
    new CodeLensProvider[F] {

      def provide(documentUri: Uri, documentText: String): List[CodeLens] =
        SourceParser[SourceFile].parse(documentText) match {
          case Right(parsed) if runner.get(parsed).isRight =>
            compiler
              .compile(parsed)
              .as {
                val queries = parsed.queries
                if (queries.isEmpty)
                  Nil
                else
                  CodeLens(
                    range = queries.head.query.value.operationName.range,
                    Command(
                      title = "Run query",
                      command = Command.RUN_QUERY,
                      args = documentUri.value :: Nil,
                    ),
                  ) :: Nil
              }
              .toList
              .flatten
          case _ => Nil
        }

    }

}

case class CodeLens(range: SourceRange, command: Command)
case class Command(title: String, command: String, args: List[String])

object Command {
  // todo: needs rename everywhere (including UI)
  val RUN_QUERY: String = "smithyql.runQuery"
}
