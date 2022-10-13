package playground.smithyql

import cats.Id
import cats.Monad
import cats.StackSafeMonad
import cats.data.NonEmptyList
import cats.implicits._

import java.io.Closeable
import scala.util.Using
import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.bindings.TreeSitterInstance

object ParserAdapter {

  implicit object CloseableIsReleasable extends Using.Releasable[Closeable] {
    def release(resource: Closeable): Unit = resource.close()
  }

  object ast {

    case class QualifiedIdentifier[F[_]](
      path: F[NonEmptyList[Identifier[F]]],
      selection: F[Identifier[F]],
    ) {

      def sequenceK(
        implicit F: Monad[F]
      ): F[QualifiedIdentifier[Id]] =
        (
          path.flatMap(_.nonEmptyTraverse(_.sequenceK)),
          selection.flatMap(_.sequenceK),
        )
          .mapN(QualifiedIdentifier.apply[Id])

    }

    case class UseClause[F[_]](
      ident: F[QualifiedIdentifier[F]]
    )

    case class Op[F[_]](
      useClause: F[Option[UseClause[F]]],
      name: F[Identifier[F]],
      input: F[Struct[F]],
    )

    case class Identifier[F[_]](value: F[String]) {
      def sequenceK(implicit F: Monad[F]): F[Identifier[Id]] = value.map(Identifier.apply[Id])
    }

    case class Field[F[_]](identifier: F[Identifier[F]], inputNode: F[InputNode[F]])
    case class Fields[F[_]](fields: F[List[Field[F]]])
    case class Struct[F[_]](fields: F[Option[Fields[F]]])
    case class ListItems[F[_]](items: F[List[InputNode[F]]])
    case class Listed[F[_]](items: F[Option[ListItems[F]]])
    sealed trait InputNode[F[_]]

    object InputNode {
      case class StructCase[F[_]](s: Struct[F]) extends InputNode[F]
      case class ListCase[F[_]](l: Listed[F]) extends InputNode[F]
      case class NumberCase[F[_]](n: F[String]) extends InputNode[F]
      case class StringCase[F[_]](n: F[String]) extends InputNode[F]
      case class BoolCase[F[_]](v: F[Boolean]) extends InputNode[F]
      case class NullCase[F[_]]() extends InputNode[F]
    }

  }

  object ast1 {

    type QualifiedIdentifier = ast.QualifiedIdentifier[Id]
    val QualifiedIdentifier = ast.QualifiedIdentifier[Id]
    type UseClause = ast.UseClause[Id]
    val UseClause = ast.UseClause[Id]
    type Op = ast.Op[Id]
    val Op = ast.Op[Id]
    type Identifier = ast.Identifier[Id]
    val Identifier = ast.Identifier[Id]
    type Field = ast.Field[Id]
    val Field = ast.Field[Id]
    type Fields = ast.Fields[Id]
    val Fields = ast.Fields[Id]
    type Struct = ast.Struct[Id]
    val Struct = ast.Struct[Id]
    type ListItems = ast.ListItems[Id]
    val ListItems = ast.ListItems[Id]
    type Listed = ast.Listed[Id]
    val Listed = ast.Listed[Id]
    type InputNode = ast.InputNode[Id]

    object InputNode {
      type StructCase = ast.InputNode.StructCase[Id]
      val StructCase = ast.InputNode.StructCase[Id]
      type ListCase = ast.InputNode.ListCase[Id]
      val ListCase = ast.InputNode.ListCase[Id]
      type NumberCase = ast.InputNode.NumberCase[Id]
      val NumberCase = ast.InputNode.NumberCase[Id]
      type StringCase = ast.InputNode.StringCase[Id]
      val StringCase = ast.InputNode.StringCase[Id]
      type BoolCase = ast.InputNode.BoolCase[Id]
      val BoolCase = ast.InputNode.BoolCase[Id]
      type NullCase = ast.InputNode.NullCase[Id]
      val NullCase = ast.InputNode.NullCase[Id]()
    }

  }

  import ast1._

  trait NodeDecoder[+A] {

    def decode(node: Node, text: String): A

    def text: NodeDecoder[String] = (node, _) => node.source

    def firstMatch[B](implicit ev: A <:< List[B]): NodeDecoder[Option[B]] = this.map {
      _.headOption
    }

    def unNone[B](
      implicit ev: A <:< Option[B]
    ): NodeDecoder[B] = this.map(_.getOrElse(sys.error("missing item")))

    def atType(
      tpe: String
    ): NodeDecoder[A] = atTypeOpt(tpe).unNone

    def atTypeOpt(
      tpe: String
    ): NodeDecoder[Option[A]] = atTypeSeq(tpe).firstMatch

    def atTypeSeq(tpe: String): NodeDecoder[List[A]] =
      (node, text) =>
        NodeDecoder
          .children
          .map(_.filter(_.tpe == tpe))
          .map(_.map(this.decode(_, text)))
          .decode(node, text)

  }

  object NodeDecoder {
    val id: NodeDecoder[Node] = (node, _) => node

    implicit val instances: Monad[NodeDecoder] =
      new StackSafeMonad[NodeDecoder] {

        def flatMap[A, B](
          fa: NodeDecoder[A]
        )(
          f: A => NodeDecoder[B]
        ): NodeDecoder[B] = { (node, text) =>
          f(fa.decode(node, text)).decode(node, text)
        }

        def pure[A](x: A): NodeDecoder[A] = (_, _) => x

      }

    val children: NodeDecoder[List[Node]] = (node, _) => node.children

    val childTypes: NodeDecoder[String] = children
      .map(_.map(_.tpe).mkString(", "))

    def union[A](decoders: (String, NodeDecoder[A])*): NodeDecoder[A] =
      (node, text) =>
        decoders
          .collectFirstSome { case (key, v) => v.atTypeOpt(key).decode(node, text) }
          .getOrElse(sys.error("missing case: " + childTypes.decode(node, text)))

    val text: NodeDecoder[String] = id.text

    implicit final class NodeDecoderOps[A](nd: => NodeDecoder[A]) {
      def deferred: NodeDecoder[A] = nd.decode(_, _)
    }

  }

  object OpDecoders {
    import NodeDecoder._
    import NodeDecoder.NodeDecoderOps

    val identifier: NodeDecoder[Identifier] = text.map(Identifier(_))

    val listItems: NodeDecoder[ListItems] = inputNode
      .atTypeSeq("input_node")
      .map(ListItems.apply)

    val listed: NodeDecoder[Listed] = listItems
      .atTypeOpt("list_fields")
      .map(Listed.apply)

    val bool: NodeDecoder[Boolean] = text
      .map(_.toBoolean)

    lazy val inputNode: NodeDecoder[InputNode] =
      union(
        "number" -> text.map(InputNode.NumberCase(_)),
        "string" -> text.map(InputNode.StringCase(_)),
        "null" -> InputNode.NullCase.pure[NodeDecoder],
        "struct" -> struct.map(InputNode.StructCase(_)),
        "list" -> listed.map(InputNode.ListCase(_)),
        "boolean" -> bool.map(InputNode.BoolCase(_)),
      ).deferred

    val field: NodeDecoder[Field] =
      (
        identifier.atType("identifier"),
        inputNode.atType("input_node"),
      ).mapN(Field.apply)

    val fields: NodeDecoder[Fields] = field
      .atTypeSeq("field")
      .map(Fields.apply)

    val struct: NodeDecoder[Struct] = fields
      .atTypeOpt("fields")
      .map(Struct.apply)

    val qualifiedIdentifier: NodeDecoder[QualifiedIdentifier] = identifier
      .atTypeSeq("identifier")
      .map {
        case _ :: Nil | Nil => sys.error("no ident??")
        case all =>
          val prefix = NonEmptyList.fromListUnsafe(all.init)

          QualifiedIdentifier(prefix, all.last)
      }

    val useClause: NodeDecoder[UseClause] = qualifiedIdentifier
      .atType("qualified_identifier")
      .map(UseClause)

    val op: NodeDecoder[Op] =
      (
        useClause.atTypeOpt("use_clause"),
        identifier.atType("operation_name"),
        struct.atType("struct"),
      ).mapN(Op.apply)

  }

  def parse(s: String) = {
    val p = TreeSitterInstance.make(SmithyQLLanguageBindings.SmithyQL)
    val tree = p.parse(s)
    println(tree.rootNode.get.fields.keySet)
    // OpDecoders.op.decode(tree.rootNode.get, s)
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
