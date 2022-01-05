package playground

import cats.Id
import cats.effect.kernel.Sync
import cats.implicits._
import playground.smithyql.SmithyQLParser
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.TextEditor
import typings.vscode.mod.window

object run {

  def perform[F[_]: Sync, Op[_, _, _, _, _]](
    ted: TextEditor,
    compiler: Compiler[Op, Id],
    runner: Runner[F, Op],
    channel: OutputChannel,
  ): F[Unit] = SmithyQLParser
    .parse(ted.document.getText())
    .liftTo[F]
    .map(compiler.compile)
    .flatMap { q =>
      Sync[F].delay {
        channel.show()
        channel.appendLine(s"> ${q.input}")
      } *>
        runner
          .run(q)
          .flatMap { out =>
            Sync[F].delay {
              channel.appendLine(s"< $out")
            }
          }
    }
    .onError { e =>
      Sync[F].delay(window.showErrorMessage(e.getMessage())).void
    }

}
