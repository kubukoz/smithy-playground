package playground

import cats.effect.kernel.Sync
import cats.implicits._
import playground.smithyql.SmithyQLParser
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.TextEditor
import typings.vscode.mod.window
import playground.smithyql.Formatter
import playground.smithyql.WithSource

object run {

  def perform[F[_]: Sync, Op[_, _, _, _, _]](
    ted: TextEditor,
    compiler: Compiler[Op, F],
    runner: Runner[F, Op],
    channel: OutputChannel,
  ): F[Unit] = SmithyQLParser
    .parseFull(ted.document.getText())
    .liftTo[F]
    .flatMap { parsed =>
      compiler
        .compile(parsed)
        .flatMap { compiled =>
          Sync[F].delay {
            channel.show()
            channel.appendLine(s"Calling ${parsed.operationName.value.text}")
          } *>
            runner
              .run(compiled)
              .onError { case e => Sync[F].delay(channel.appendLine("ERROR " + e.toString())) }
              .flatMap { out =>
                Sync[F].delay {
                  channel.appendLine(s"Succeeded ${parsed.operationName.value.text}, response:\n")
                  channel.appendLine(Formatter.writeAst(out.mapK(WithSource.liftId)).renderTrim(80))
                }
              }
        }
    }

}
