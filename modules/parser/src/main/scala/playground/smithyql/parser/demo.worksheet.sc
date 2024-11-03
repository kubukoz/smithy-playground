import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.*

val s =
  """
use service foo.bar.baz.bax#Baz

Bax { a = , x = 44  y = 50, b = ,,42, xyz = { a = }}
    """.stripMargin

val p = TreeSitterAPI.make("smithyql")

val tree = p.parse(s)

tree.rootNode.get.text

SourceFile(tree.rootNode.get).use_clause.get.identifier.head.node.source

SourceFile(tree.rootNode.get).use_clause.get.identifier.selection.node.source

SourceFile(tree.rootNode.get)
  .use_clause
  .get
  .identifier
  .tail

SourceFile(tree.rootNode.get)
  .statements
  .operation_call
  .input
  .children
  .filter(_.tpe == "ERROR")
  .head
  .source

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.tpe == "ERROR")
  .map(_.source)

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.isMissing)

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.isExtra)
  .size

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.isError)
  .map(_.text)
  .mkString("\n")

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .size

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.hasError)
  .size

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  // .filter(_.tpe == "MISSING")
  // .map(_.source)
  .map(_.tpe)
  .toSet

val bind =
  SourceFile(tree.rootNode.get)
    .statements
    .operation_call
    .input
    .bindings
    .get
    .binding

// .find(_.key.source == "x")
// .get

bind.size
bind.head.fields
bind.map(_.key.source)

bind.last.value.asStruct.get.bindings.get.binding.head.value.asBoolean.get.tpe
// bind.key.source

// bind.value.asNumber.get.source
