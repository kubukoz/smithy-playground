// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class ListFields /* private */(node: Node) extends Node {

  def typedChildren: List[InputNode] = node.children.toList.collect {
    case node @ InputNode() => InputNode(node)
  }

  export node.*
}

object ListFields {
  def unapply(node: Node): Boolean = node.tpe == "list_fields"
}

/*

*/
