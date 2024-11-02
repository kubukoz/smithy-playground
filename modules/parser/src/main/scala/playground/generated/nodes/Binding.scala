// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Binding /* private */(node: Node) extends Node {

  def key: Identifier = node.fields("key").head match {
    case node @ Identifier() => Identifier(node)
  }
  def value: _Input_node = node.fields("value").head match {
    case node @ _Input_node() => _Input_node(node)
  }

  export node.*
}

object Binding {
  def unapply(node: Node): Boolean = node.tpe == "binding"
}

/*

*/
