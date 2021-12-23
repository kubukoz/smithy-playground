import typings.vscode.mod.ExtensionContext

import typings.vscode.mod.window

import scala.scalajs.js.annotation.JSExportTopLevel
import typings.vscode.mod.workspace
import typings.vscode.mod.NotebookDocumentContentOptions
import typings.vscode.mod.NotebookSerializer
import cats.effect.IO

import cats.effect.unsafe.implicits._

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
    import cats.implicits._
    IO.println("Hello from cats-effect!")
      .unsafeRunAndForget()
    println("Registered serializer")
  }

}
