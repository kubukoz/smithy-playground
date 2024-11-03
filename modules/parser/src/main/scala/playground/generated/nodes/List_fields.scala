// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class List_fields /* private */(node: Node) extends Node {


  def typedChildren: List[_Input_node] = node.children.toList.collect {
    case node @ _Input_node() => _Input_node(node)
  }
  export node.*
}

object List_fields {
  def unapply(node: Node): Boolean = node.tpe == "list_fields"
}

/*

*/
