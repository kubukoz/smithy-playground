// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class QualifiedIdentifier /* private */(node: Node) extends Node {
  // fields
  def head: Option[Identifier] = node.fields.getOrElse("head", Nil).headOption.map {
    case Identifier(node) => node
  }

  def selection: Option[Identifier] = node.fields.getOrElse("selection", Nil).headOption.map {
    case Identifier(node) => node
  }

  def tail: List[Identifier] = node.fields.getOrElse("tail", Nil).toList.collect {
    case Identifier(node) => node
  }
  // typed children

  // precise typed children


  export node.*
}

object QualifiedIdentifier {
  def apply(node: Node): Either[String, QualifiedIdentifier] =
    if node.tpe == "qualified_identifier"
    then Right(new QualifiedIdentifier(node))
    else Left(s"Expected QualifiedIdentifier, got ${node.tpe}")
  def unsafeApply(node: Node): QualifiedIdentifier = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[QualifiedIdentifier] = apply(node).toOption
}

/*

*/
