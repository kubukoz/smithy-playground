// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class QualifiedIdentifier /* private */(node: Node) extends Node {
  def head: Identifier = node.fields("head").head match {
    case node @ Identifier() => Identifier(node)
  }

  def selection: Identifier = node.fields("selection").head match {
    case node @ Identifier() => Identifier(node)
  }

  def tail: List[Identifier] = node.fields("tail").toList.collect {
    case node @ Identifier() => Identifier(node)
  }


  export node.*
}

object QualifiedIdentifier {
  def unapply(node: Node): Boolean = node.tpe == "qualified_identifier"
}

/*

*/
