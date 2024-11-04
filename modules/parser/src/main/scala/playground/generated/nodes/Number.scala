// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class Number /* private */(node: Node) extends Node {
  // fields

  // typed children

  // precise typed children


  export node.*
}

object Number {
  def apply(node: Node): Either[String, Number] =
    if node.tpe == "number"
    then Right(new Number(node))
    else Left(s"Expected Number, got ${node.tpe}")
  def unsafeApply(node: Node): Number = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Number] = apply(node).toOption
}

/*

*/
