// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class List_ /* private */(node: Node) extends Node {
  // fields
  def list_fields: ListFields = node.fields("list_fields").head match {
    case node @ ListFields() => ListFields(node)
  }
  // typed children

  // precise typed children


  export node.*
}

object List_ {
  def unapply(node: Node): Boolean = node.tpe == "list"
}

/*

*/
