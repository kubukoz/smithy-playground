// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class Comment /* private */(node: Node) extends Node {
  // fields

  // typed children

  // precise typed children


  export node.*
}

object Comment {
  def apply(node: Node): Either[String, Comment] =
    if node.tpe == "comment"
    then Right(new Comment(node))
    else Left(s"Expected Comment, got ${node.tpe}")
  def unsafeApply(node: Node): Comment = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Comment] = apply(node).toOption
}

/*

*/
