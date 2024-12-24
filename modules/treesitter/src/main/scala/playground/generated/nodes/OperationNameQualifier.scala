// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type OperationNameQualifier <: Node = Node

object OperationNameQualifier {
  extension (node: OperationNameQualifier) {
    def select[A](f: OperationNameQualifier.Selector => Selection[A]): List[A] = f(OperationNameQualifier.Selector(List(node))).path
    // fields

    // typed children
    def typedChildren: Option[QualifiedIdentifier] = node.children.collectFirst {
      case QualifiedIdentifier(node) => node
    }
    // precise typed children
    def qualified_identifier: Option[QualifiedIdentifier] = node.children.collectFirst {
      case QualifiedIdentifier(node) => node
    }
  }

  def apply(node: Node): Either[String, OperationNameQualifier] =
    if node.tpe == "operation_name_qualifier"
    then Right(node)
    else Left(s"Expected OperationNameQualifier, got ${node.tpe}")

  def unsafeApply(node: Node): OperationNameQualifier = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[OperationNameQualifier] = apply(node).toOption

  final case class Selector(path: List[OperationNameQualifier]) extends Selection[OperationNameQualifier] {
    def qualified_identifier: QualifiedIdentifier.Selector = QualifiedIdentifier.Selector(path.flatMap(_.qualified_identifier))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
