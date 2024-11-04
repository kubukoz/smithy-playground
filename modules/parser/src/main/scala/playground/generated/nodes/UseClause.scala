// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class UseClause /* private */(node: Node) extends Node {
  // fields
  def identifier: Option[QualifiedIdentifier] = node.fields.getOrElse("identifier", Nil).headOption.map {
    case QualifiedIdentifier(node) => node
  }
  // typed children
  def typedChildren: List[Whitespace] = node.children.toList.collect {
    case Whitespace(node) => node
  }
  // precise typed children
  def whitespace: List[Whitespace] = node.children.toList.collect {
    case Whitespace(node) => node
  }

  export node.*
}

object UseClause {
  def apply(node: Node): Either[String, UseClause] =
    if node.tpe == "use_clause"
    then Right(new UseClause(node))
    else Left(s"Expected UseClause, got ${node.tpe}")
  def unsafeApply(node: Node): UseClause = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[UseClause] = apply(node).toOption
}

/*

*/
