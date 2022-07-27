package playground

import scalajs.js
import org.scalablytyped.runtime.StObject
import typings.vscodeLanguageclient.clientMod.LanguageClientOptions
import typings.vscode.anon.Dispose
import scala.annotation.nowarn

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
