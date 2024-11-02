// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Qualified_identifier /* private */(node: Node) extends Node {

  def head: Identifier = Identifier(node.fields("head").head)
  def selection: Identifier = Identifier(node.fields("selection").head)
  def tail: List[Identifier] = node.fields("tail").toList.collect {
    case node @ Identifier() => Identifier(node)
  }

  export node.*
}

object Qualified_identifier {
  def unapply(node: Node): scala.Boolean = node.tpe == "qualified_identifier"
}

/*

*/
