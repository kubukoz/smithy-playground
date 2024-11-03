// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class ListFields /* private */(node: Node) extends Node {
  // fields

  // typed children
  def typedChildren: List[InputNode] = node.children.toList.collect {
    case node @ InputNode() => InputNode(node)
  }
  // precise typed children
  def _input_node: List[InputNode] = node.children.toList.collect {
    case node @ InputNode() => InputNode(node)
  }

  export node.*
}

object ListFields {
  def unapply(node: Node): Boolean = node.tpe == "list_fields"
}

/*

*/
