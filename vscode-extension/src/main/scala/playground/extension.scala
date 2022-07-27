package playground

import org.scalablytyped.runtime.StObject
import typings.vscode.anon.Dispose
import typings.vscode.mod
import typings.vscode.mod.ExtensionContext
import typings.vscodeLanguageclient.clientMod.LanguageClientOptions
import typings.vscodeLanguageserverProtocol.protocolMod

import scala.annotation.nowarn
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.JSConverters._
import scalajs.js
import typings.vscodeLanguageclient.clientMod.RevealOutputChannelOn
import typings.std.stdStrings
import typings.vscodeLanguageclient.clientMod.BaseLanguageClient

@js.native
// "dead code"
@nowarn()
trait ServerOptions extends StObject {
  var command: String = js.native
  var args: js.UndefOr[js.Array[String]] = js.native
}

object ServerOptions {

  @scala.inline
  def apply(command: String, args: js.UndefOr[js.Array[String]]): ServerOptions = {
    val __obj = js
      .Dynamic
      .literal(command = command, args = args)
    __obj.asInstanceOf[ServerOptions]
  }

}

@js.native
@js.annotation.JSImport("vscode-languageclient/node", "LanguageClient")
@nowarn("cat=unused")
class LanguageClient(
  id: String,
  name: String,
  serverOptions: ServerOptions,
  clientOptions: LanguageClientOptions,
) extends js.Object {
  def start(): Dispose = js.native
}

object extension {

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    val lspClient =
      new LanguageClient(
        "smithyPlayground",
        "Smithy Playground",
        ServerOptions(
          "/Users/kubukoz/projects/smithy-playground/lsp/target/jvm-2.13/universal/stage/bin/lsp",
          List(
            "-J-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,quiet=y,address=5005"
          ).toJSArray,
        ),
        LanguageClientOptions()
          .setDocumentSelectorVarargs(
            mod
              .DocumentFilter()
              .setLanguage("smithyql")
              .asInstanceOf[protocolMod.DocumentFilter]
          ),
      )

    val lspClientFull = lspClient
      .asInstanceOf[BaseLanguageClient]

    def disposableToDispose(disposable: mod.Disposable): Dispose = Dispose(disposable.dispose)

    val registerRunCommand = disposableToDispose(
      mod
        .commands
        .registerTextEditorCommand(
          "smithyql.runQuery",
          (editor, _, _) => {
            lspClientFull
              .sendRequest(
                "smithyql/runQuery",
                js.Dynamic
                  .literal(
                    uri = editor.document.uri.toString()
                  ),
              )
            ()
          },
        )
    )

    val registerOutputPanelNotification = disposableToDispose(
      lspClientFull.onNotification(
        "smithyql/showOutputPanel",
        (_: Any) => lspClientFull.outputChannel.show(true),
      )
    )

    context
      .subscriptions
      .push(
        lspClient.start(),
        registerRunCommand,
        registerOutputPanelNotification,
      )
    ()
  }

}
