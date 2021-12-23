import typings.vscode.mod.ExtensionContext

import typings.vscode.mod.window

import scala.scalajs.js.annotation.JSExportTopLevel
import typings.vscode.mod.workspace
import typings.vscode.mod.NotebookDocumentContentOptions
import typings.vscode.mod.NotebookSerializer

object extension {
  // val chan = window.createOutputChannel("Smithy Playground")

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {

    workspace.registerNotebookSerializer(
      "smithyql",
      NotebookSerializer(
        deserializeNotebook =
          (bytes, cancellation) => {
            println(bytes)
            ???
          },
        serializeNotebook =
          (nbd, cancellation) =>
            // chan.appendLine("watter?")
            {
              println((nbd, cancellation))
              ???
            },
      ),
    )
    println("Registered serializer")
  }

}
