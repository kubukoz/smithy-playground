// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Use_clause /* private */(node: Node) extends Node {

  def identifier: Qualified_identifier = Qualified_identifier(node.fields("identifier").head)

  export node.*
}

object Use_clause {
  def unapply(node: Node): scala.Boolean = node.tpe == "use_clause"
}

/*

*/
