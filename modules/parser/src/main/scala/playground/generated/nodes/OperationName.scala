// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class OperationName /* private */(node: Node) extends Node {
  // fields
  def identifier: List[QualifiedIdentifier] = node.fields.getOrElse("identifier", Nil).toList.collect {
    case QualifiedIdentifier(node) => node
  }

  def name: Option[Identifier] = node.fields.getOrElse("name", Nil).headOption.map {
    case Identifier(node) => node
  }
  // typed children

  // precise typed children


  export node.*
}

object OperationName {
  def apply(node: Node): Either[String, OperationName] =
    if node.tpe == "operation_name"
    then Right(new OperationName(node))
    else Left(s"Expected OperationName, got ${node.tpe}")
  def unsafeApply(node: Node): OperationName = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[OperationName] = apply(node).toOption
}

/*

*/
