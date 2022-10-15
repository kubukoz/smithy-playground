package playground.plugins

import smithy4s.http4s.SimpleProtocolBuilder

trait PlaygroundPlugin {
  def http4sBuilders: List[SimpleProtocolBuilder[_]]
}
