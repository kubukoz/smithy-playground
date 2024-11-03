// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class LetBinding /* private */(node: Node) extends Node {

  def typedChildren: List[Binding | Whitespace] = node.children.toList.collect {
    case node @ Binding() => Binding(node)
    case node @ Whitespace() => Whitespace(node)
  }

  export node.*
}

object LetBinding {
  def unapply(node: Node): Boolean = node.tpe == "let_binding"
}

/*

*/
