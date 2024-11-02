// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Binding /* private */(node: Node) extends Node {

  def key: Identifier = Identifier(node.fields("key").head)
  def value: _Input_node = _Input_node(node.fields("value").head)

  export node.*
}

object Binding {
  def unapply(node: Node): scala.Boolean = node.tpe == "binding"
}

/*

*/
