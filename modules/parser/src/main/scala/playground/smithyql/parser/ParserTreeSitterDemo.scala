package playground.smithyql

import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.*

object ParserTreeSitterDemo extends App {

  val tree = TreeSitterAPI.make("smithyql").parse {
    """
    use service foo.bar.bax.qux#Baz

    Bax { x = 42 }
    """.stripMargin
  }

  val bind = SourceFile(tree.rootNode.get)
    .statements
    .operation_call
    .input
    .bindings
    .getOrElse(sys.error("no bindings in bindings section of struct"))
    .binding
    .find(_.key.source == "x")
    .getOrElse(sys.error("no binding with key 'x'"))

  println(bind.value.asNumber.getOrElse(sys.error("binding wasn't a number literal")).source)
}
