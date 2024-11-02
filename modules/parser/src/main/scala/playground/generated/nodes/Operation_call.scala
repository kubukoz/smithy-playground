// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Operation_call /* private */(node: Node) extends Node {

  def input: Struct = node.fields("input").head match {
    case node @ Struct() => Struct(node)
  }
  def operation_name: Operation_name = node.fields("operation_name").head match {
    case node @ Operation_name() => Operation_name(node)
  }

  export node.*
}

object Operation_call {
  def unapply(node: Node): Boolean = node.tpe == "operation_call"
}

/*

*/
