// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Operation_name /* private */(node: Node) extends Node {

  def identifier: List[Qualified_identifier] = node.fields("identifier").toList.collect {
    case node @ Qualified_identifier() => Qualified_identifier(node)
  }
  def name: Identifier = node.fields("name").head match {
    case node @ Identifier() => Identifier(node)
  }

  export node.*
}

object Operation_name {
  def unapply(node: Node): Boolean = node.tpe == "operation_name"
}

/*

*/
