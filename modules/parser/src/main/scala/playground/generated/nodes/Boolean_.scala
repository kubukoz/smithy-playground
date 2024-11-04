// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class Boolean_ /* private */(node: Node) extends Node {
  // fields

  // typed children

  // precise typed children


  export node.*
}

object Boolean_ {
  def apply(node: Node): Either[String, Boolean_] =
    if node.tpe == "boolean"
    then Right(new Boolean_(node))
    else Left(s"Expected Boolean_, got ${node.tpe}")
  def unsafeApply(node: Node): Boolean_ = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Boolean_] = apply(node).toOption
}

/*

*/
