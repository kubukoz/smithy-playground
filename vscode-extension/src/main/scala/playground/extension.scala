package playground

import org.scalablytyped.runtime.StObject
import typings.vscode.anon.Dispose
import typings.vscode.mod
import typings.vscode.mod.ExtensionContext
import typings.vscodeLanguageclient.clientMod.LanguageClientOptions
import typings.vscodeLanguageserverProtocol.protocolMod

import scala.annotation.nowarn
import scala.scalajs.js.annotation.JSExportTopLevel

import scalajs.js

@js.native
// "dead code"
@nowarn()
trait ServerOptions extends StObject {
  var command: String = js.native
}

object ServerOptions {

  @scala.inline
  def apply(command: String): ServerOptions = {
    val __obj = js.Dynamic.literal(command = command.asInstanceOf[js.Any])
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
          "/Users/kubukoz/projects/smithy-playground/lsp/target/jvm-2.13/universal/stage/bin/lsp"
        ),
        LanguageClientOptions().setDocumentSelectorVarargs(
          mod
            .DocumentFilter()
            .setLanguage("smithyql")
            .asInstanceOf[protocolMod.DocumentFilter]
        ),
      )

    context.subscriptions.push(lspClient.start())
    ()
  }

}
