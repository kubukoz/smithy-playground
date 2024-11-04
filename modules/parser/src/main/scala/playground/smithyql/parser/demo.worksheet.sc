import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.*

val s =
  """
use service foo.bar.baz.bax#Baz

Bax { a = , x = 44  y = 50, b = ,,42, xyz = { a = }}
    """.stripMargin

val p = TreeSitterAPI.make("smithyql")

SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).tpe
SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).statements
SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).isError
SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).hasError
SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).children.map(_.tpe)

val tree = p.parse(s)

tree.rootNode.get.text

SourceFile(tree.rootNode.get).use_clause.get.identifier.head.source

SourceFile(tree.rootNode.get).use_clause.get.identifier.get.selection.get.source

SourceFile(tree.rootNode.get)
  .use_clause
  .get
  .identifier
  .tail

SourceFile(tree.rootNode.get)
  .statements
  .get
  .operation_call
  .get
  .input
  .get
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
    .get
    .operation_call
    .get
    .input
    .get
    .bindings
    .get
    .binding

// .find(_.key.source == "x")
// .get

bind.size
bind.head.fields
bind.map(_.key.get.source)

bind.last.value.get.asStruct.get.bindings.get.binding.head.value.get.asBoolean.get.tpe
// bind.key.source

// bind.value.asNumber.get.source
