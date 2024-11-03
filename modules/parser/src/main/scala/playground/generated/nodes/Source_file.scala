// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Source_file /* private */(node: Node) extends Node {
  def statements: Top_level_statement = node.fields("statements").head match {
    case node @ Top_level_statement() => Top_level_statement(node)
  }

  def use_clause: Use_clause = node.fields("use_clause").head match {
    case node @ Use_clause() => Use_clause(node)
  }


  export node.*
}

object Source_file {
  def unapply(node: Node): Boolean = node.tpe == "source_file"
}

/*

*/
