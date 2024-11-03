// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Use_clause /* private */(node: Node) extends Node {

  def identifier: Qualified_identifier = node.fields("identifier").head match {
    case node @ Qualified_identifier() => Qualified_identifier(node)
  }
  def typedChildren: List[Whitespace] = node.children.toList.collect {
    case node @ Whitespace() => Whitespace(node)
  }
  export node.*
}

object Use_clause {
  def unapply(node: Node): Boolean = node.tpe == "use_clause"
}

/*

*/
