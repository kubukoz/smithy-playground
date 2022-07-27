package playground

import typings.vscode.mod
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.window
import typings.vscodeLanguageclient.clientMod.LanguageClientOptions
import typings.vscodeLanguageserverProtocol.protocolMod

import scala.scalajs.js.annotation.JSExportTopLevel

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground", "smithyql")

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    val lspClient =
      new LanguageClient(
        "smithyPlayground",
        "Smithy Playground Client",
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
    chan.appendLine("Connected client")
  }

}
