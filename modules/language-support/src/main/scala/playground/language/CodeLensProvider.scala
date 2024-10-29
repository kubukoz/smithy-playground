package playground.language

import cats.syntax.all.*
import playground.FileCompiler
import playground.FileRunner
import playground.smithyql.SourceFile
import playground.smithyql.SourceRange
import playground.smithyql.WithSource
import playground.smithyql.parser.SourceParser
import playground.types.*

trait CodeLensProvider[F[_]] {

  def provide(
    documentUri: Uri,
    documentText: String,
  ): List[CodeLens]

}

object CodeLensProvider {

  def instance[F[_]](
    compiler: FileCompiler[IorThrow],
    runner: FileRunner.Resolver[F],
  ): CodeLensProvider[F] =
    new CodeLensProvider[F] {

      def provide(
        documentUri: Uri,
        documentText: String,
      ): List[CodeLens] =
        if (FileNames.isOutputPanel(documentUri.value))
          Nil
        else
          provideImpl(documentUri, documentText)

      private def provideImpl(
        documentUri: Uri,
        documentText: String,
      ): List[CodeLens] =
        SourceParser[SourceFile].parse(documentText) match {
          case Right(parsed) if runner.get(parsed).isRight =>
            compiler
              .compile(parsed)
              .as {
                val queries = parsed.queries(WithSource.unwrap)
                if (queries.isEmpty)
                  Nil
                else
                  CodeLens(
                    range = queries.head.query.value.operationName.range,
                    Command(
                      title = "Run SmithyQL file",
                      command = Command.RUN_FILE,
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

case class CodeLens(
  range: SourceRange,
  command: Command,
)

case class Command(
  title: String,
  command: String,
  args: List[String],
)

object Command {
  // Legacy value, kept for backwards compatibility with the language client
  val RUN_FILE: String = "smithyql.runQuery"
}
