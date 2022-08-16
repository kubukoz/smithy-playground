package playground.smithyql

import ai.serenade.treesitter.Languages
import ai.serenade.treesitter.Node
import ai.serenade.treesitter.Parser
import cats.implicits._

import java.io.Closeable
import scala.util.Using
import cats.data.NonEmptyList

object ParserAdapter {

  System.load("/Users/kubukoz/projects/java-tree-sitter/out.dylib")

  implicit object CloseableIsReleasable extends Using.Releasable[Closeable] {
    def release(resource: Closeable): Unit = resource.close()
  }

  def children(node: Node): List[Node] = List.tabulate(node.getChildCount())(node.getChild(_))

  def downType(
    tpe: String
  )(
    node: Node
  ): Node = downTypeOpt(tpe)(node)
    .getOrElse(
      sys.error(
        "no children with type " + tpe + ", existing types: " + children(node)
          .map(_.getType())
          .mkString(",")
      )
    )

  def downTypeAll(
    tpe: String
  )(
    node: Node
  ): List[Node] = children(node).filter(_.getType() == tpe)

  def downTypeOpt(
    tpe: String
  )(
    node: Node
  ): Option[Node] = children(node)
    .find(_.getType() == tpe)

  def childTypes(
    node: Node
  ): String = children(node)
    .map(_.getType())
    .mkString(", ")

  case class QualifiedIdentifier(
    path: NonEmptyList[Identifier],
    selection: Identifier,
  )

  case class UseClause(
    ident: QualifiedIdentifier
  )

  case class Op(
    useClause: Option[UseClause],
    name: String,
    input: Struct,
  )

  case class Identifier(value: String)

  case class Field(identifier: Identifier, inputNode: InputNode)
  case class Fields(fields: List[Field])
  case class Struct(fields: Option[Fields])
  case class ListItems(items: List[InputNode])
  case class Listed(items: Option[ListItems])
  sealed trait InputNode

  object InputNode {
    case class StructCase(s: Struct) extends InputNode
    case class ListCase(l: Listed) extends InputNode
    case class NumberCase(n: String) extends InputNode
    case class StringCase(n: String) extends InputNode
    case class BoolCase(v: Boolean) extends InputNode
    case object NullCase extends InputNode
  }

  def union[A](decoders: (String, Node => A)*): Node => A =
    node =>
      decoders
        .collectFirstSome { case (key, v) => downTypeOpt(key)(node).map(v) }
        .getOrElse(sys.error("missing case: " + childTypes(node)))

  class OpDecoders(src: String) {
    def text(node: Node) = src.substring(node.getStartByte(), node.getEndByte())

    val identifier: Node => Identifier = text.map(Identifier(_))

    val listItems: Node => ListItems = downTypeAll("input_node")
      .map(_.map(inputNode))
      .map(ListItems.apply)

    val listed: Node => Listed = downTypeOpt("list_fields")
      .map(_.map(listItems))
      .map(Listed.apply)

    val bool: Node => Boolean = text
      .map(_.toBoolean)

    lazy val inputNode: Node => InputNode = union(
      "number" -> text.map(InputNode.NumberCase(_)),
      "string" -> text.map(InputNode.StringCase(_)),
      "null" -> Function.const(InputNode.NullCase),
      "struct" -> struct.map(InputNode.StructCase(_)),
      "list" -> listed.map(InputNode.ListCase(_)),
      "boolean" -> bool.map(InputNode.BoolCase(_)),
    )

    val field: Node => Field =
      (
        downType("identifier") >>> identifier,
        downType("input_node") >>> inputNode,
      ).mapN(Field.apply)

    val fields: Node => Fields = downTypeAll("field")
      .map(_.map(field))
      .map(Fields.apply)

    lazy val struct: Node => Struct = downTypeOpt("fields")
      .map(_.map(fields))
      .map(Struct.apply)

    val qualifiedIdentifier: Node => QualifiedIdentifier = downTypeAll("identifier")
      .map {
        case _ :: Nil | Nil => sys.error("no ident??")
        case all =>
          val prefix = NonEmptyList.fromListUnsafe(all.init)

          QualifiedIdentifier(prefix.map(identifier), identifier(all.last))
      }

    val useClause: Node => UseClause =
      (
        downType("qualified_identifier") >>>
          qualifiedIdentifier
      )
        .map(UseClause)

    val op: Node => Op =
      (
        downTypeOpt("use_clause").map(_.map(useClause)),
        downType("operation_name") >>>
          downType("identifier") >>>
          text,
        downType("struct") >>>
          struct,
      ).mapN(Op.apply)

  }

  def parse(s: String) = {

    val p = new Parser()
    p.setLanguage(Languages.smithyql())
    Using.resource(p.parseString(s)) { tree =>
      new OpDecoders(s).op(tree.getRootNode())
    }
  }

}

object ParserAdapterDemo extends App {
  println(
    ParserAdapter
      .parse(
        "use service a.b#C\n helalsdfhl //a\n{ hello = 42, foo = 50, x = { y = \"hello\"} , z = null, aa = [10, true, false, null]}"
      )
  )
}
