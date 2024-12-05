// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type SourceFile <: Node = Node

object SourceFile {
  extension (node: SourceFile) {
    def select[A](f: SourceFile.Selector => Selection[A]): List[A] = f(SourceFile.Selector(List(node))).path
    // fields
    def prelude: Option[Prelude] = node.fields.getOrElse("prelude", Nil).headOption.map {
      case Prelude(node) => node
    }

    def statements: List[TopLevelStatement] = node.fields.getOrElse("statements", Nil).toList.collect {
      case TopLevelStatement(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, SourceFile] =
    if node.tpe == "source_file"
    then Right(node)
    else Left(s"Expected SourceFile, got ${node.tpe}")

  def unsafeApply(node: Node): SourceFile = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[SourceFile] = apply(node).toOption

  final case class Selector(path: List[SourceFile]) extends Selection[SourceFile] {
    def prelude: Prelude.Selector = Prelude.Selector(path.flatMap(_.prelude))
    def statements: TopLevelStatement.Selector = TopLevelStatement.Selector(path.flatMap(_.statements))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
