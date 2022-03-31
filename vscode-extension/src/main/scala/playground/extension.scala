package playground

import cats.effect.IO
import cats.effect.kernel.Deferred
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
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.api.SimpleRestJson
import smithy4s.SchemaIndex
import smithy4s.dynamic.model.Model
import io.scalajs.nodejs.child_process.SpawnSyncResult

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground")

  // todo move somewhere else
  // todo autoreload etc etc
  val service: DynamicSchemaIndex.ServiceWrapper = {

    import io.scalajs.nodejs.child_process.ChildProcess
    import io.scalajs.nodejs.child_process.Output

    val process: Output = ChildProcess.execSync(
      "/nix/store/m5igl1nk1wblx5alzj8r2l56awnwgyvk-smithy4s-codegen-0.12.7/bin/smithy4s-codegen",
      scalajs
        .js
        .Array(
          "dump-model",
          "/Users/kubukoz/projects/smithy-playground/core/src/main/smithy/demo.smithy",
        ),
    )

    val modelText =
      (process: Any) match {
        case b: io.scalajs.nodejs.buffer.Buffer => b.toString("UTF-8")
        case s: String                          => s
      }

    val capi = smithy4s.http.json.codecs()

    DynamicSchemaIndex
      .load(
        capi.decodeFromByteArray(capi.compileCodec(Model.schema), modelText.getBytes()).toTry.get,
        SimpleRestJson
          .protocol
          .schemas ++
          // todo: should this be included?
          SchemaIndex(SimpleRestJson),
      )
      .allServices
      .head
  }

  implicit val compiler: Compiler[service.Op, EitherThrow] = Compiler.instance(service.service)

  implicit val runner: Runner[IO, service.Op] = {
    val runnerDeff = Deferred.unsafe[IO, Runner[IO, service.Op]]

    client
      .make[IO](useNetwork = false)
      .flatMap(Runner.make(service.service, _))
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

    val completionProvider = completions.complete(service.service)

    val _ = context
      .subscriptions
      .push(
        commands
          .registerTextEditorCommand(
            "smithyql.runQuery",
            (ted, _, _) =>
              run
                .perform[IO, service.Op](ted, compiler.mapK(eitherToIO), runner, chan)
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
              .full[service.Op, EitherThrow](doc.getText())
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
        vscodeutil.registerDiagnosticProvider("smithyql", highlight.getHighlights[service.Op]),
      )
  }

}
