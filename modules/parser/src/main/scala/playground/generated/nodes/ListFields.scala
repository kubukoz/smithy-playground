// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class ListFields /* private */(node: Node) extends Node {
  // fields

  // typed children
  def typedChildren: List[InputNode] = node.children.toList.collect {
    case InputNode(node) => node
  }
  // precise typed children
  def _input_node: List[InputNode] = node.children.toList.collect {
    case InputNode(node) => node
  }

  export node.*
}

object ListFields {
  def apply(node: Node): Either[String, ListFields] =
    if node.tpe == "list_fields"
    then Right(new ListFields(node))
    else Left(s"Expected ListFields, got ${node.tpe}")
  def unsafeApply(node: Node): ListFields = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[ListFields] = apply(node).toOption
}

/*

*/
