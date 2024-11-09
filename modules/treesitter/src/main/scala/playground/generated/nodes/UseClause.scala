// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type UseClause <: Node = Node

object UseClause {
  extension (node: UseClause) {
    // fields
    def identifier: Option[QualifiedIdentifier] = node.fields.getOrElse("identifier", Nil).headOption.map {
      case QualifiedIdentifier(node) => node
    }
    // typed children
    def typedChildren: List[Whitespace] = node.children.toList.collect {
      case Whitespace(node) => node
    }
    // precise typed children
    def whitespace: List[Whitespace] = node.children.toList.collect {
      case Whitespace(node) => node
    }
  }

  def apply(node: Node): Either[String, UseClause] =
    if node.tpe == "use_clause"
    then Right(node)
    else Left(s"Expected UseClause, got ${node.tpe}")
  def unsafeApply(node: Node): UseClause = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[UseClause] = apply(node).toOption
}

/*

*/
