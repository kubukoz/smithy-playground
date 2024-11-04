// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class Bindings /* private */(node: Node) extends Node {
  // fields

  // typed children
  def typedChildren: List[Binding] = node.children.toList.collect {
    case Binding(node) => node
  }
  // precise typed children
  def binding: List[Binding] = node.children.toList.collect {
    case Binding(node) => node
  }

  export node.*
}

object Bindings {
  def apply(node: Node): Either[String, Bindings] =
    if node.tpe == "bindings"
    then Right(new Bindings(node))
    else Left(s"Expected Bindings, got ${node.tpe}")
  def unsafeApply(node: Node): Bindings = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Bindings] = apply(node).toOption
}

/*

*/
