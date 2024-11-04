// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class Whitespace /* private */(node: Node) extends Node {
  // fields

  // typed children

  // precise typed children


  export node.*
}

object Whitespace {
  def apply(node: Node): Either[String, Whitespace] =
    if node.tpe == "whitespace"
    then Right(new Whitespace(node))
    else Left(s"Expected Whitespace, got ${node.tpe}")
  def unsafeApply(node: Node): Whitespace = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Whitespace] = apply(node).toOption
}

/*

*/
