// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class List_ /* private */(node: Node) extends Node {
  // fields
  def list_fields: Option[ListFields] = node.fields.getOrElse("list_fields", Nil).headOption.map {
    case ListFields(node) => node
  }
  // typed children

  // precise typed children


  export node.*
}

object List_ {
  def apply(node: Node): Either[String, List_] =
    if node.tpe == "list"
    then Right(new List_(node))
    else Left(s"Expected List_, got ${node.tpe}")
  def unsafeApply(node: Node): List_ = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[List_] = apply(node).toOption
}

/*

*/
