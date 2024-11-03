// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class SourceFile /* private */(node: Node) extends Node {
  // fields
  def statements: TopLevelStatement = node.fields("statements").head match {
    case node @ TopLevelStatement() => TopLevelStatement(node)
  }

  def use_clause: UseClause = node.fields("use_clause").head match {
    case node @ UseClause() => UseClause(node)
  }
  // typed children

  // precise typed children


  export node.*
}

object SourceFile {
  def unapply(node: Node): Boolean = node.tpe == "source_file"
}

/*

*/
