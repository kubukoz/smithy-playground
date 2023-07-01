package playground.sample

import playground.plugins.PlaygroundPlugin
import playground.plugins.SimpleHttpBuilder

class SamplePlaygroundPlugin extends PlaygroundPlugin {
  override def simpleBuilders: List[SimpleHttpBuilder] = Nil
}
