package playground

import org.scalablytyped.runtime.StObject
import typings.vscode.anon.Dispose
import typings.vscode.mod
import typings.vscode.mod.ExtensionContext
import typings.vscodeLanguageclient.clientMod.BaseLanguageClient
import typings.vscodeLanguageclient.clientMod.LanguageClientOptions
import typings.vscodeLanguageserverProtocol.protocolMod

import scala.annotation.nowarn
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

import scalajs.js

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
    val serverArtifact =
      mod
        .workspace
        .getConfiguration()
        .get[String]("smithyql.server.artifact")
        .orNull

    val serverVersion =
      mod
        .workspace
        .getConfiguration()
        .get[String]("smithyql.server.version")
        .orNull

    val lspClient =
      new LanguageClient(
        "smithyPlayground",
        "Smithy Playground",
        ServerOptions(
          "cs",
          List(
            "launch",
            s"${serverArtifact}:$serverVersion",
            "--",
            "-J-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,quiet=y,address=5005",
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
