// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class UseClause /* private */(node: Node) extends Node {
  def identifier: QualifiedIdentifier = node.fields("identifier").head match {
    case node @ QualifiedIdentifier() => QualifiedIdentifier(node)
  }
  def typedChildren: List[Whitespace] = node.children.toList.collect {
    case node @ Whitespace() => Whitespace(node)
  }

  export node.*
}

object UseClause {
  def unapply(node: Node): Boolean = node.tpe == "use_clause"
}

/*

*/
