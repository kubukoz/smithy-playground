package playground.smithyql

import org.polyvariant.treesitter4s.Node

object tsutils {

  extension (node: Node) {
    def range: SourceRange = SourceRange(Position(node.startByte), Position(node.endByte))
  }

}
