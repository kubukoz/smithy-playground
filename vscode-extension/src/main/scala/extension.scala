import typings.vscode.mod.ExtensionContext

import typings.vscode.mod.window

import scala.scalajs.js.annotation.JSExportTopLevel

object extension {
  val chan = window.createOutputChannel("Smithy Playground")

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = chan.appendLine("foo")

}
