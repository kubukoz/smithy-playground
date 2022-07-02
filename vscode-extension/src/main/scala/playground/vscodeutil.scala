package playground

import typings.vscode.mod.Diagnostic
import typings.vscode.mod.Disposable
import typings.vscode.mod.TextDocument
import typings.vscode.mod.languages
import typings.vscode.mod.window
import typings.vscode.mod.workspace

import scala.scalajs.js
import typings.vscode.anon.Dispose
import cats.effect.kernel.Sync

object vscodeutil {
  implicit def disposableToDispose(d: Disposable): Dispose = Dispose(() => d.dispose())

  def registerDiagnosticProvider(
    language: String,
    provider: (TextDocument) => List[Diagnostic],
  ): Disposable = {
    val coll = languages.createDiagnosticCollection()

    def performHighlight(doc: TextDocument): Unit =
      if (doc.languageId == language) {
        val highlights = provider(doc)
        coll.set(doc.uri, js.Array(highlights: _*))
      }

    window.activeTextEditor.foreach { ted =>
      performHighlight(ted.document)
    }

    val disposables = List(
      window.onDidChangeActiveTextEditor(
        _.map(_.document).foreach(performHighlight),
        null,
        null,
      ),
      workspace
        .onDidSaveTextDocument
        .apply(
          performHighlight(_),
          (),
          (),
        ),
      workspace.onDidCloseTextDocument(
        doc => coll.delete(doc.uri),
        null,
        null,
      ),
    )

    new Disposable(() => disposables.foreach(_.dispose()))
  }

  def unsafeGetConfig[A](key: String): A = workspace.getConfiguration().get[A](key).get
  def getConfigF[F[_]: Sync, A](key: String): F[A] = Sync[F].delay(unsafeGetConfig[A](key))

}
