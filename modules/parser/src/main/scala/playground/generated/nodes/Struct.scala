// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Struct /* private */(node: Node) extends Node {
  // fields
  def bindings: Option[Bindings] = node.fields("bindings").headOption.map {
    case node @ Bindings() => Bindings(node)
  }
  // typed children

  // precise typed children


  export node.*
}

object Struct {
  def unapply(node: Node): Boolean = node.tpe == "struct"
}

/*

*/
