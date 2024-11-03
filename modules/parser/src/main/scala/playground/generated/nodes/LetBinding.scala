// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class LetBinding /* private */(node: Node) extends Node {
  // fields

  // typed children
  def typedChildren: List[Binding | Whitespace] = node.children.toList.collect {
    case node @ Binding() => Binding(node)
    case node @ Whitespace() => Whitespace(node)
  }
  // precise typed children
  def binding: List[Binding] = node.children.toList.collect {
    case node @ Binding() => Binding(node)
  }

  def whitespace: List[Whitespace] = node.children.toList.collect {
    case node @ Whitespace() => Whitespace(node)
  }

  export node.*
}

object LetBinding {
  def unapply(node: Node): Boolean = node.tpe == "let_binding"
}

/*

*/
