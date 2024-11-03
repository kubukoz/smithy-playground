// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class OperationName /* private */(node: Node) extends Node {
  def identifier: List[QualifiedIdentifier] = node.fields("identifier").toList.collect {
    case node @ QualifiedIdentifier() => QualifiedIdentifier(node)
  }

  def name: Identifier = node.fields("name").head match {
    case node @ Identifier() => Identifier(node)
  }


  export node.*
}

object OperationName {
  def unapply(node: Node): Boolean = node.tpe == "operation_name"
}

/*

*/
