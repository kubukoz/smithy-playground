// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type OperationName <: Node = Node

object OperationName {
  extension (node: OperationName) {
    def select[A](f: OperationName.Selector => Selection[A]): List[A] = f(OperationName.Selector(List(node))).path
    // fields
    def identifier: List[QualifiedIdentifier] = node.fields.getOrElse("identifier", Nil).toList.collect {
      case QualifiedIdentifier(node) => node
    }

    def name: Option[Identifier] = node.fields.getOrElse("name", Nil).headOption.map {
      case Identifier(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, OperationName] =
    if node.tpe == "operation_name"
    then Right(node)
    else Left(s"Expected OperationName, got ${node.tpe}")

  def unsafeApply(node: Node): OperationName = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[OperationName] = apply(node).toOption

  final case class Selector(path: List[OperationName]) extends Selection[OperationName] {
    def identifier: QualifiedIdentifier.Selector = QualifiedIdentifier.Selector(path.flatMap(_.identifier))
    def name: Identifier.Selector = Identifier.Selector(path.flatMap(_.name))
  }
}
