// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class String_ /* private */(node: Node) extends Node {
  // fields

  // typed children

  // precise typed children


  export node.*
}

object String_ {
  def apply(node: Node): Either[String, String_] =
    if node.tpe == "string"
    then Right(new String_(node))
    else Left(s"Expected String_, got ${node.tpe}")
  def unsafeApply(node: Node): String_ = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[String_] = apply(node).toOption
}

/*

*/
