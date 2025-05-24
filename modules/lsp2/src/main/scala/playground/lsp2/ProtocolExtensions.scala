package playground.lsp2

import langoustine.lsp.requests.CustomNotification
import langoustine.lsp.requests.CustomRequest
import langoustine.lsp.runtime.DocumentUri
import upickle.default.*

object ProtocolExtensions {

  object smithyql {
    object showOutputPanel extends CustomNotification[Unit]("smithyql/showOutputPanel")
    object runQuery extends CustomRequest[RunQueryParams, Unit]("smithyql/runQuery")

    case class RunQueryParams(uri: DocumentUri) derives ReadWriter
  }

}
