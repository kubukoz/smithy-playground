package playground

import cats.effect.IO
import cats.effect.implicits._
import cats.effect.unsafe.implicits._
import cats.implicits._
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
import smithy4s.Service
import cats.effect.std

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground")

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = build
    .buildFile[IO]
    .toResource
    .map { buildFile =>
      build.getService(buildFile)
    }
    .flatMap { service =>
      client
        .make[IO](useNetwork = false)
        .flatMap { client =>
          Runner
            .make(service.service, client)
            .evalMap(implicit runner => IO(activateInternal(context, service.service)))
        }
    }
    .allocated
    .onError { case e => std.Console[IO].printStackTrace(e) }
    .unsafeRunAndForget()

  def activateInternal[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    context: ExtensionContext,
    service: Service[Alg, Op],
  )(
    implicit runner: Runner[IO, Op]
  ): Unit = {
    implicit val compiler: Compiler[Op, EitherThrow] = Compiler.instance(
      service.service
    )

    import vscodeutil.disposableToDispose

    val completionProvider = completions.complete(service.service)

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
            .CompletionItemProvider { (doc, pos, _, _) =>
              completionProvider(doc, pos).toJSArray
            },
          // todo this might not be working properly
          "\t",
        ),
        languages.registerCodeLensProvider(
          "smithyql",
          mod.CodeLensProvider { (doc, _) =>
            validate
              .full[Op, EitherThrow](doc.getText())
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
        vscodeutil.registerDiagnosticProvider("smithyql", highlight.getHighlights[Op]),
      )
  }

}
