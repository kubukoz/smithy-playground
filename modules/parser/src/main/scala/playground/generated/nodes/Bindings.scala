// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Bindings /* private */(node: Node) extends Node {

  def typedChildren: List[Binding] = node.children.toList.collect {
    case node @ Binding() => Binding(node)
  }

  export node.*
}

object Bindings {
  def unapply(node: Node): Boolean = node.tpe == "bindings"
}

/*

*/
