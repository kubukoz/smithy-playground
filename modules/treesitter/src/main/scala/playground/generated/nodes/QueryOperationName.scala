// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type QueryOperationName <: Node = Node

object QueryOperationName {
  extension (node: QueryOperationName) {
    def select[A](f: QueryOperationName.Selector => Selection[A]): List[A] = f(QueryOperationName.Selector(List(node))).path
    // fields
    def identifier: Option[OperationNameQualifier] = node.fields.getOrElse("identifier", Nil).headOption.map {
      case OperationNameQualifier(node) => node
    }

    def name: Option[OperationName] = node.fields.getOrElse("name", Nil).headOption.map {
      case OperationName(node) => node
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
    def identifier: OperationNameQualifier.Selector = OperationNameQualifier.Selector(path.flatMap(_.identifier))
    def name: OperationName.Selector = OperationName.Selector(path.flatMap(_.name))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
