package playground

import playground.smithyql.SourceRange
import playground.smithyql.SmithyQLParser
import cats.implicits._
import types._

trait CodeLensProvider[F[_]] {
  def provide(documentUri: String, documentText: String): List[CodeLens]
}

object CodeLensProvider {

  def instance[F[_]](
    compiler: Compiler[IorThrow],
    runner: Runner.Optional[F],
  ): CodeLensProvider[F] =
    new CodeLensProvider[F] {

      def provide(documentUri: String, documentText: String): List[CodeLens] =
        SmithyQLParser.parseFull(documentText) match {
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
