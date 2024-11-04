// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class QualifiedIdentifier /* private */(node: Node) extends Node {
  // fields
  def head: Option[Identifier] = node.fields.getOrElse("head", Nil).headOption.map {
    case node @ Identifier() => Identifier(node)
  }

  def selection: Option[Identifier] = node.fields.getOrElse("selection", Nil).headOption.map {
    case node @ Identifier() => Identifier(node)
  }

  def tail: List[Identifier] = node.fields.getOrElse("tail", Nil).toList.collect {
    case node @ Identifier() => Identifier(node)
  }
  // typed children

  // precise typed children


  export node.*
}

object QualifiedIdentifier {
  def unapply(node: Node): Boolean = node.tpe == "qualified_identifier"
}

/*

*/
