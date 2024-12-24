// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type QueryOperationName <: Node = Node

object QueryOperationName {
  extension (node: QueryOperationName) {
    def select[A](f: QueryOperationName.Selector => Selection[A]): List[A] = f(QueryOperationName.Selector(List(node))).path
    // fields
    def name: Option[OperationName] = node.fields.getOrElse("name", Nil).headOption.map {
      case OperationName(node) => node
    }

    def service_identifier: Option[QualifiedIdentifier] = node.fields.getOrElse("service_identifier", Nil).headOption.map {
      case QualifiedIdentifier(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, QueryOperationName] =
    if node.tpe == "query_operation_name"
    then Right(node)
    else Left(s"Expected QueryOperationName, got ${node.tpe}")

  def unsafeApply(node: Node): QueryOperationName = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[QueryOperationName] = apply(node).toOption

  final case class Selector(path: List[QueryOperationName]) extends Selection[QueryOperationName] {
    def name: OperationName.Selector = OperationName.Selector(path.flatMap(_.name))
    def service_identifier: QualifiedIdentifier.Selector = QualifiedIdentifier.Selector(path.flatMap(_.service_identifier))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
