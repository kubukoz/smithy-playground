package playground

import typings.vscode.mod.TextEditor
import cats.effect.kernel.Sync
import cats.implicits._
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.window

object run {

  def perform[F[_]: Sync](ted: TextEditor, runner: Runner[F], channel: OutputChannel): F[Unit] =
    Sync[F]
      .delay(SmithyQLParser.parse(ted.document.getText()))
      .flatMap(runner.run)
      .flatMap { out =>
        Sync[F].delay {
          channel.appendLine(out.toString)
        }
      }
      .onError { e =>
        Sync[F].delay(window.showErrorMessage(e.getMessage())).void
      }

}
