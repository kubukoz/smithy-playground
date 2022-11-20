package playground.lsp

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services

trait PlaygroundLanguageClient extends services.LanguageClient {
  @JsonNotification("smithyql/showOutputPanel")
  def showOutputPanel(): Unit
}
