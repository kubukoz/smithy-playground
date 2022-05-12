package playground

import cats.effect.kernel.Sync
import cats.implicits._
import playground.smithyql.SmithyQLParser
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.TextEditor
import playground.smithyql.Formatter
import playground.smithyql.WithSource
import playground.smithyql.InputNode

object run {

  def perform[F[_]: Sync, Op[_, _, _, _, _]](
    ted: TextEditor,
    compiler: Compiler[F],
    runner: Runner[F],
    channel: OutputChannel,
  ): F[Unit] = SmithyQLParser
    .parseFull(ted.document.getText())
    .liftTo[F]
    .flatMap { parsed =>
      compiler
        .compile(parsed)
        .flatMap { compiled =>
          Sync[F].delay {
            channel.show(true)
            channel.appendLine(s"Calling ${parsed.operationName.value.text}")
          } *>
            // todo: this ideally wouldn't throw
            Sync[F]
              .defer(
                runner
                  .run(compiled)
              )
              .onError { case e =>
                val rendered =
                  compiled
                    .catchError(e)
                    .flatMap(err => compiled.writeError.map(_.toNode(err))) match {
                    case Some(e) => writeOutput(e)
                    case None    => e.toString
                  }

                Sync[F].delay(channel.appendLine("ERROR " + rendered))
              }
              .flatMap { out =>
                Sync[F].delay {
                  channel.appendLine(s"Succeeded ${parsed.operationName.value.text}, response:\n")
                  channel.appendLine(writeOutput(out))
                }
              }
        }
    }

  private def writeOutput(
    node: InputNode[cats.Id]
  ) = Formatter.writeAst(node.mapK(WithSource.liftId)).renderTrim(80)

}
