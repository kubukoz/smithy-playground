// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class OperationCall /* private */(node: Node) extends Node {
  // fields
  def input: Struct = node.fields("input").head match {
    case node @ Struct() => Struct(node)
  }

  def operation_name: OperationName = node.fields("operation_name").head match {
    case node @ OperationName() => OperationName(node)
  }
  // typed children

  // precise typed children


  export node.*
}

object OperationCall {
  def unapply(node: Node): Boolean = node.tpe == "operation_call"
}

/*

*/
