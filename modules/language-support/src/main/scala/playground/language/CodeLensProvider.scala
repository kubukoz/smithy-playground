package playground.language

import playground.smithyql.SourceRange
import cats.implicits._
import playground.types._
import playground.OperationCompiler
import playground.OperationRunner
import playground.smithyql.parser.SourceParser
import playground.smithyql.Query

trait CodeLensProvider[F[_]] {
  def provide(documentUri: String, documentText: String): List[CodeLens]
}

object CodeLensProvider {

  def instance[F[_]](
    compiler: OperationCompiler[IorThrow],
    runner: OperationRunner.Resolver[F],
  ): CodeLensProvider[F] =
    new CodeLensProvider[F] {

      def provide(documentUri: String, documentText: String): List[CodeLens] =
        SourceParser[Query].parse(documentText) match {
          case Right(parsed) if runner.get(parsed).toEither.isRight =>
            compiler
              .compile(parsed)
              .as {
                CodeLens(
                  range = parsed.operationName.range,
                  Command(
                    title = "Run query",
                    command = Command.RUN_QUERY,
                    args = documentUri :: Nil,
                  ),
                )
              }
              .toList
          case _ => Nil
        }

    }

}

case class CodeLens(range: SourceRange, command: Command)
case class Command(title: String, command: String, args: List[String])

object Command {
  val RUN_QUERY: String = "smithyql.runQuery"
}
