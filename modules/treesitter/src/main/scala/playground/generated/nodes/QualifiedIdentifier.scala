// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type QualifiedIdentifier <: Node = Node

object QualifiedIdentifier {
  extension (node: QualifiedIdentifier) {
    // fields
    def namespace: List[Identifier] = node.fields.getOrElse("namespace", Nil).toList.collect {
      case Identifier(node) => node
    }

    def selection: Option[Identifier] = node.fields.getOrElse("selection", Nil).headOption.map {
      case Identifier(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, QualifiedIdentifier] =
    if node.tpe == "qualified_identifier"
    then Right(node)
    else Left(s"Expected QualifiedIdentifier, got ${node.tpe}")
  def unsafeApply(node: Node): QualifiedIdentifier = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[QualifiedIdentifier] = apply(node).toOption
}
