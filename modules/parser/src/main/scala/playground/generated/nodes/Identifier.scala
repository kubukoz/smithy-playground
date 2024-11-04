// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class Identifier /* private */(node: Node) extends Node {
  // fields

  // typed children

  // precise typed children


  export node.*
}

object Identifier {
  def apply(node: Node): Either[String, Identifier] =
    if node.tpe == "identifier"
    then Right(new Identifier(node))
    else Left(s"Expected Identifier, got ${node.tpe}")
  def unsafeApply(node: Node): Identifier = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Identifier] = apply(node).toOption
}

/*

*/
