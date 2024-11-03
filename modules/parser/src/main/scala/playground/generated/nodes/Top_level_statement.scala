// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Top_level_statement /* private */(node: Node) extends Node {


  def typedChildren: Let_binding | Operation_call = node.children.head match {
    case node @ Let_binding() => Let_binding(node)
    case node @ Operation_call() => Operation_call(node)
  }
  export node.*
}

object Top_level_statement {
  def unapply(node: Node): Boolean = node.tpe == "top_level_statement"
}

/*

*/
