// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class Struct /* private */(node: Node) extends Node {
  // fields
  def bindings: Option[Bindings] = node.fields.getOrElse("bindings", Nil).headOption.map {
    case Bindings(node) => node
  }
  // typed children

  // precise typed children


  export node.*
}

object Struct {
  def apply(node: Node): Either[String, Struct] =
    if node.tpe == "struct"
    then Right(new Struct(node))
    else Left(s"Expected Struct, got ${node.tpe}")
  def unsafeApply(node: Node): Struct = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Struct] = apply(node).toOption
}

/*

*/
