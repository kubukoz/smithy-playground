// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class TopLevelStatement /* private */(node: Node) extends Node {
  // fields

  // typed children
  def typedChildren: Option[LetBinding | OperationCall] = node.children.collectFirst {
    case node @ LetBinding() => LetBinding(node)
    case node @ OperationCall() => OperationCall(node)
  }
  // precise typed children
  def let_binding: Option[LetBinding] = node.children.collectFirst {
    case node @ LetBinding() => LetBinding(node)
  }

  def operation_call: Option[OperationCall] = node.children.collectFirst {
    case node @ OperationCall() => OperationCall(node)
  }

  export node.*
}

object TopLevelStatement {
  def unapply(node: Node): Boolean = node.tpe == "top_level_statement"
}

/*

*/
