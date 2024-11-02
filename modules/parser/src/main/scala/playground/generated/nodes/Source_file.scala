// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Source_file /* private */(node: Node) extends Node {

  def statements: Top_level_statement = Top_level_statement(node.fields("statements").head)
  def use_clause: Use_clause = Use_clause(node.fields("use_clause").head)

  export node.*
}

object Source_file {
  def unapply(node: Node): scala.Boolean = node.tpe == "source_file"
}

/*

*/
