import cats.effect.IO

import cats.effect.unsafe.implicits._
import typings.vscode.anon.Dispose
import typings.vscode.mod.Disposable
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.commands
import typings.vscode.mod.window

import scala.scalajs.js.annotation.JSExportTopLevel

object extension {
  // val chan = window.createOutputChannel("Smithy Playground")
  implicit def disposableToDispose(d: Disposable): Dispose = Dispose(() => d.dispose())

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    /*
     "contributes": {
    "notebooks": [
      {
        "type": "smithyql",
        "displayName": "SmithyQL",
        "selector": [
          {
            "filenamePattern": "*.smithyql"
          }
        ]
      }
    ]
  }, */
    // workspace.registerNotebookSerializer(
    //   "smithyql",
    //   NotebookSerializer(
    //     deserializeNotebook =
    //       (bytes, cancellation) => {
    //         println(bytes)
    //         println(42)
    //         ???
    //       },
    //     serializeNotebook =
    //       (nbd, cancellation) =>
    //         // chan.appendLine("watter?")
    //         {
    //           println((nbd, cancellation))
    //           ???
    //         },
    //   ),
    // )
    // register command

    val _ = context
      .subscriptions
      .push(
        commands
          .registerTextEditorCommand(
            "smithyql.runQuery",
            (ted, edit, x) => {
              window.showErrorMessage("Here goes nothing!")
              println("Here goes nothing!")
              println(ted.document.getText())
              ()
            },
          )
      )
    println("starting")
    // languages.registerDocumentFormattingEditProvider()
    IO(window.showInformationMessage("Hello from cats-effect!"))
      .unsafeRunAndForget()

  }

}
