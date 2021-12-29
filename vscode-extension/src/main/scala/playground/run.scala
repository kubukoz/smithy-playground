package playground

import typings.vscode.mod.TextEditor
import cats.effect.kernel.Sync
import cats.implicits._
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.window
import cats.Id

object run {

  def perform[F[_]: Sync, Op[_, _, _, _, _]](
    ted: TextEditor,
    compiler: Compiler[Op, Id],
    runner: Runner[F, Op],
    channel: OutputChannel,
  ): F[Unit] = Sync[F]
    .delay(SmithyQLParser.parse(ted.document.getText()))
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
