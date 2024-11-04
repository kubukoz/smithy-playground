// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class LetBinding /* private */(node: Node) extends Node {
  // fields

  // typed children
  def typedChildren: List[Binding | Whitespace] = node.children.toList.collect {
    case Binding(node) => node
    case Whitespace(node) => node
  }
  // precise typed children
  def binding: List[Binding] = node.children.toList.collect {
    case Binding(node) => node
  }

  def whitespace: List[Whitespace] = node.children.toList.collect {
    case Whitespace(node) => node
  }

  export node.*
}

object LetBinding {
  def apply(node: Node): Either[String, LetBinding] =
    if node.tpe == "let_binding"
    then Right(new LetBinding(node))
    else Left(s"Expected LetBinding, got ${node.tpe}")
  def unsafeApply(node: Node): LetBinding = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[LetBinding] = apply(node).toOption
}

/*

*/
