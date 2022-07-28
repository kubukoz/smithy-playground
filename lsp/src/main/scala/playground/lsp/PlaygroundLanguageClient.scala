package playground.lsp

import org.eclipse.lsp4j.services
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

trait PlaygroundLanguageClient extends services.LanguageClient {
  @JsonNotification("smithyql/showOutputPanel")
  def showOutputPanel(): Unit
}
