// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type SourceFile <: Node = Node

object SourceFile {
  extension (node: SourceFile) {
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
}
