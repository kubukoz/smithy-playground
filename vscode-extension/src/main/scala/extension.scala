import cats.effect.IO

import cats.effect.kernel.Deferred
import cats.effect.unsafe.implicits._
import com.disneystreaming.demo.smithy.CreateHeroOutput
import com.disneystreaming.demo.smithy.CreateSubscriptionOutput
import com.disneystreaming.demo.smithy.DemoService
import com.disneystreaming.demo.smithy.DemoServiceGen
import com.disneystreaming.demo.smithy.Hero
import com.disneystreaming.demo.smithy.Subscription
import playground.Formatter
import playground.Runner
import playground.SmithyQLParser
import smithy4s.http4s.SimpleRestJsonBuilder
import typings.vscode.anon.Dispose
import typings.vscode.mod
import typings.vscode.mod.Disposable
import typings.vscode.mod.DocumentFormattingEditProvider
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.TextEdit
import typings.vscode.mod.commands
import typings.vscode.mod.languages
import typings.vscode.mod.window

import scala.scalajs.js.annotation.JSExportTopLevel

object extension {
  val chan = window.createOutputChannel("Smithy Playground")
  val runner = Deferred.unsafe[IO, Runner[IO]]

  val mkRunner = SimpleRestJsonBuilder
    .routes(new DemoService[IO] {

      override def createHero(
        hero: Hero
      ): IO[CreateHeroOutput] = IO(CreateHeroOutput(hero))

      override def createSubscription(
        subscription: Subscription
      ): IO[CreateSubscriptionOutput] = IO(CreateSubscriptionOutput(subscription))

    })
    .resource
    .flatMap { routes =>
      Runner
        .make(DemoServiceGen, Some(routes.orNotFound))
    }

  implicit def disposableToDispose(d: Disposable): Dispose = Dispose(() => d.dispose())

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    mkRunner
      .evalMap(runner.complete(_))
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
              IO(SmithyQLParser.parse(ted.document.getText()))
                .flatMap { q =>
                  runner
                    .get
                    .flatMap(_.run(q))
                }
                .flatMap { out =>
                  IO {
                    chan.appendLine(out.toString)
                  }
                }
                .onError { e =>
                  IO(window.showErrorMessage(e.getMessage())).void
                }
                .unsafeRunAndForget(),
          ),
        languages.registerDocumentFormattingEditProvider(
          "smithyql",
          DocumentFormattingEditProvider { (doc, options, canc) =>
            val firstLine = doc.lineAt(0)
            val lastLine = doc.lineAt(doc.lineCount - 1)

            scalajs
              .js
              .Array(
                TextEdit.replace(
                  new mod.Range(
                    firstLine.range.start,
                    lastLine.range.end,
                  ),
                  Formatter.format(SmithyQLParser.parse(doc.getText()), 40),
                )
              )
          },
        ),
      )
    println("starting")
    // languages.registerDocumentFormattingEditProvider()
    IO(window.showInformationMessage("Hello from cats-effect!"))
      .unsafeRunAndForget()

  }

}
