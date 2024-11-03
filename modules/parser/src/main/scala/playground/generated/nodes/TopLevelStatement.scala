// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class TopLevelStatement /* private */(node: Node) extends Node {
  // fields

  // typed children
  def typedChildren: LetBinding | OperationCall = node.children.head match {
    case node @ LetBinding() => LetBinding(node)
    case node @ OperationCall() => OperationCall(node)
  }
  // precise typed children
  def let_binding: LetBinding = node.children.collectFirst {
    case node @ LetBinding() => LetBinding(node)
  }.get

  def operation_call: OperationCall = node.children.collectFirst {
    case node @ OperationCall() => OperationCall(node)
  }.get

  export node.*
}

object TopLevelStatement {
  def unapply(node: Node): Boolean = node.tpe == "top_level_statement"
}

/*

*/
