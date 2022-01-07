package playground

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.implicits._
import cats.implicits._
import demo.smithy.DemoServiceGen
import playground.Runner
import typings.vscode.mod
import typings.vscode.mod.DocumentFormattingEditProvider
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.commands
import typings.vscode.mod.languages
import typings.vscode.mod.window

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

import types._

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground")

  implicit val compiler: Compiler[Op, EitherThrow] = Compiler.instance(DemoServiceGen)

  implicit val runner: Runner[IO, Op] = {
    val runnerDeff = Deferred.unsafe[IO, Runner[IO, Op]]

    client
      .make[IO](useNetwork = false)
      .flatMap(Runner.make(DemoServiceGen, _))
      .evalMap(runnerDeff.complete(_))
      .allocated
      .unsafeRunAndForget()

    Runner.unlift(runnerDeff.get)
  }

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {

    import vscodeutil.disposableToDispose

    val completionProvider = completions.complete(DemoServiceGen)

    val _ = context
      .subscriptions
      .push(
        commands
          .registerTextEditorCommand(
            "smithyql.runQuery",
            (ted, _, _) =>
              run
                .perform[IO, Op](ted, compiler.mapK(eitherToIO), runner, chan)
                .unsafeRunAndForget(),
          ),
        languages.registerCompletionItemProvider(
          "smithyql",
          mod
            .CompletionItemProvider[mod.CompletionItem] { (doc, pos, _, _) =>
              completionProvider(doc, pos).toJSArray
            },
          // todo this might not be working properly
          "\t",
        ),
        languages.registerCodeLensProvider(
          "smithyql",
          mod.CodeLensProvider { (doc, _) =>
            validate
              .full[EitherThrow](doc.getText())
              .map { case (parsed, _) =>
                new mod.CodeLens(
                  adapters.toVscodeRange(doc, parsed.operationName.range),
                  mod.Command("smithyql.runQuery", "Run query"),
                )
              }
              .toList
              .toJSArray
          },
        ),
        languages.registerDocumentFormattingEditProvider(
          "smithyql",
          DocumentFormattingEditProvider { (doc, _, _) =>
            format.perform(doc).toJSArray
          },
        ),
        vscodeutil.registerDiagnosticProvider("smithyql", highlight.getHighlights),
      )
  }

}
