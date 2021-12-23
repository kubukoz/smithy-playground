import cats.effect.IO

import cats.effect.unsafe.implicits._
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.NotebookSerializer
import typings.vscode.mod.window
import typings.vscode.mod.workspace

import scala.scalajs.js.annotation.JSExportTopLevel

object extension {
  val chan = window.createOutputChannel("Smithy Playground")

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
    IO(chan.appendLine("Hello from cats-effect!"))
      .unsafeRunAndForget()
    println("Registered serializer")
  }

}
