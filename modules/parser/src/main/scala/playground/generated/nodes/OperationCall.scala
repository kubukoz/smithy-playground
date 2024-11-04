// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class OperationCall /* private */(node: Node) extends Node {
  // fields
  def input: Option[Struct] = node.fields.getOrElse("input", Nil).headOption.map {
    case node @ Struct() => Struct(node)
  }

  def operation_name: Option[OperationName] = node.fields.getOrElse("operation_name", Nil).headOption.map {
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
