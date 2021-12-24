import cats.effect.IO

import cats.effect.unsafe.implicits._
import typings.vscode.anon.Dispose
import typings.vscode.mod.Disposable
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.commands
import typings.vscode.mod.window

import scala.scalajs.js.annotation.JSExportTopLevel
import org.http4s.ember.client.EmberClientBuilder
import smithy4s.http4s.SimpleRestJsonBuilder
import com.disneystreaming.demo.smithy.PlaygroundService
import com.disneystreaming.demo.smithy.PlaygroundServiceGen
import scala.scalajs.js.Thenable
import cats.effect.kernel.Deferred

object extension {
  val chan = window.createOutputChannel("Smithy Playground")
  val client = Deferred.unsafe[IO, PlaygroundService[IO]]

  implicit def disposableToDispose(d: Disposable): Dispose = Dispose(() => d.dispose())

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    EmberClientBuilder
      .default[IO]
      .build
      .flatMap { c =>
        import org.http4s.implicits._
        SimpleRestJsonBuilder(PlaygroundServiceGen)
          .clientResource(c, uri"http://localhost:4000")
          .evalMap { svc =>
            client.complete(svc)
          }
      }
      .allocated
      .unsafeRunAndForget()
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
            (ted, edit, x) =>
              client
                .get
                .flatMap(_.runQuery(ted.document.getText()))
                .flatMap { out =>
                  IO {
                    chan.appendLine(out.output)
                  }
                }
                .unsafeRunAndForget(),
          )
      )
    println("starting")
    // languages.registerDocumentFormattingEditProvider()
    IO(window.showInformationMessage("Hello from cats-effect!"))
      .unsafeRunAndForget()

  }

}
