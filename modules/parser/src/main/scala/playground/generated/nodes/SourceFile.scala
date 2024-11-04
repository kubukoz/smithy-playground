// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

final case class SourceFile /* private */(node: Node) extends Node {
  // fields
  def statements: Option[TopLevelStatement] = node.fields.getOrElse("statements", Nil).headOption.map {
    case TopLevelStatement(node) => node
  }

  def use_clause: Option[UseClause] = node.fields.getOrElse("use_clause", Nil).headOption.map {
    case UseClause(node) => node
  }
  // typed children

  // precise typed children


  export node.*
}

object SourceFile {
  def apply(node: Node): Either[String, SourceFile] =
    if node.tpe == "source_file"
    then Right(new SourceFile(node))
    else Left(s"Expected SourceFile, got ${node.tpe}")
  def unsafeApply(node: Node): SourceFile = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[SourceFile] = apply(node).toOption
}

/*

*/
