// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Operation_call /* private */(node: Node) extends Node {

  def input: Struct = Struct(node.fields("input").head)
  def operation_name: Operation_name = Operation_name(node.fields("operation_name").head)

  export node.*
}

object Operation_call {
  def unapply(node: Node): scala.Boolean = node.tpe == "operation_call"
}

/*

*/
