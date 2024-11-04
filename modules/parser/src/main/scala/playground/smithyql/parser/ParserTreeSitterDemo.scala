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
    .getOrElse(sys.error("no statements section in source file"))
    .operation_call
    .getOrElse(sys.error("no operation call in statements section"))
    .input
    .getOrElse(sys.error("no input in operation call"))
    .bindings
    .getOrElse(sys.error("no bindings in bindings section of struct"))
    .binding
    .find(_.key.exists(_.source == "x"))
    .getOrElse(sys.error("no binding with key 'x'"))

  println(
    bind
      .value
      .getOrElse(sys.error("binding doesn't have a value"))
      .asNumber
      .getOrElse(sys.error("binding wasn't a number literal"))
      .source
  )
}
