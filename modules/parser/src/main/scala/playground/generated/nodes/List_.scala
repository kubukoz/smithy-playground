// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class List_ /* private */(node: Node) extends Node {
  def list_fields: List_fields = node.fields("list_fields").head match {
    case node @ List_fields() => List_fields(node)
  }


  export node.*
}

object List_ {
  def unapply(node: Node): Boolean = node.tpe == "list"
}

/*

*/
