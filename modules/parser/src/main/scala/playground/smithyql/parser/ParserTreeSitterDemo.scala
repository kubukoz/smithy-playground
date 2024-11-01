package playground.smithyql

import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI

object ParserTreeSitterDemo extends App {

  def parse(s: String) = {
    val p = TreeSitterAPI.make("smithyql")
    val tree = p.parse(s)
    println(
      tree
        .rootNode
        .get
        .fold[LazyList[Node]](_ #:: _.to(LazyList).flatten)
        .filter(_.fields.nonEmpty)
        .find(_.source == "Bax")
        .get
        .parents
        .map(p => p.text)
        .mkString("\n\n")
    )
  }

  parse(
    """
    use service foo.bar#Baz

    Bax { x = 42 }
    """.stripMargin
  )
}
