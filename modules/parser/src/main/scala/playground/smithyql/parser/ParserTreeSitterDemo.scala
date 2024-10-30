package playground.smithyql

import org.polyvariant.treesitter4s.TreeSitterAPI

object ParserTreeSitterDemo extends App {

  def parse(s: String) = {
    val p = TreeSitterAPI.make("smithyql")
    val tree = p.parse(s)
    println(tree.rootNode.get.fields.keySet)
  }

  println(
    parse(
      "use service a.b#C\n helalsdfhl //a\n{ hello = 42, foo = 50, x = { y = \"hello\"} , z = null, aa = [10, true, false, null]}"
    )
  )
}
