package playground.parsergen

import cats.data.NonEmptyList
import cats.syntax.all.*
import monocle.syntax.all.*
import org.polyvariant.treesitter4s.Node
import smithy4s.Blob
import smithy4s.json.Json
import treesittersmithy.FieldName
import treesittersmithy.NodeType
import treesittersmithy.NodeTypes
import treesittersmithy.TypeName
import util.chaining.*

import java.nio.file.Files
import java.nio.file.Paths
import scala.annotation.targetName
import scala.jdk.CollectionConverters.*
import scala.meta.Dialect

extension (tn: TypeName) {
  @targetName("renderTypeName")
  def render: String = tn.value.dropWhile(_ == '_').fromSnakeCase.ident
  def renderProjection: String = show"as${tn.value.dropWhile(_ == '_').fromSnakeCase}".ident
  def asChildName: FieldName = FieldName(tn.value)
}

extension (fn: FieldName) {
  @targetName("renderFieldName")
  def render: String = fn.value.ident
}

extension (tpe: NodeType) {

  def render: String =
    IR.from(tpe) match {
      case union: Type.Union     => renderUnion(union)
      case product: Type.Product => renderProduct(product)
    }

}

private def renderUnion(u: Type.Union): String = {
  val name = u.name.render
  val underlyingType = u.subtypes.map(_.name.render).mkString_(" | ")

  val projections = u.subtypes.map { sub =>
    // format: off
    show"""def ${sub.name.renderProjection}: Option[${sub.name.render}] = ${sub.name.render}.unapply(node)"""
    // format: on
  }

  val instanceMethods =
    show"""extension (node: $name) {
          |${projections.mkString_("\n").indentTrim(2)}
          |}""".stripMargin

  val applyMethod = {
    val cases = u
      .subtypes
      .map(nodeType => show"""case ${nodeType.name.render}(node) => Right(node)""")

    show"""def apply(node: Node): Either[String, $name] = node match {
          |${cases.mkString_("\n").indentTrim(2)}
          |  case _ => Left(s"Expected $name, got $${node.tpe}")
          |}""".stripMargin
  }

  val typedApplyMethod = show"""def apply(node: $underlyingType): $name = node""".stripMargin

  val selectorMethods = u
    .subtypes
    .map { subtype =>
      // format: off
      show"""def ${subtype.name.asChildName.render} : ${subtype.name.render}.Selector = ${subtype.name.render}.Selector(path.flatMap(_.${subtype.name.renderProjection}))"""
      // format: on
    }
    .mkString_("\n")

  show"""// Generated code! Do not modify by hand.
        |package playground.generated.nodes
        |
        |import ${classOf[Node].getName()}
        |import playground.treesitter4s.std.Selection
        |
        |opaque type $name <: Node = $underlyingType
        |
        |object $name {
        |
        |${instanceMethods.indentTrim(2)}
        |
        |${applyMethod.indentTrim(2)}
        |
        |${typedApplyMethod.indentTrim(2)}
        |
        |  def unsafeApply(node: Node): $name = apply(node).fold(sys.error, identity)
        |
        |  def unapply(node: Node): Option[$name] = apply(node).toOption
        |
        |  final case class Selector(path: List[$name]) extends Selection[$name] {
        |${selectorMethods.indentTrim(4)}
        |
        |    type Self = Selector
        |    protected val remake = Selector.apply
        |  }
        |}
        |""".stripMargin
}

private def renderProduct(p: Type.Product): String = {
  val name = p.name.render

  def renderTypeUnion(types: NonEmptyList[TypeName]) = types
    .map(_.render)
    .reduceLeft(_ + " | " + _)

  def renderFieldType(field: Field): String = renderTypeUnion(field.targetTypes).pipe {
    case s if field.repeated => show"List[$s]"
    case s                   => show"Option[$s]"
  }

  def renderChildrenType(children: Children): String = renderTypeUnion(children.targetTypes).pipe {
    case s if children.repeated => show"List[$s]"
    case s                      => show"Option[$s]"
  }

  def renderChildType(tpe: TypeName, repeated: Boolean): String = tpe.render.pipe {
    case s if repeated => show"List[$s]"
    case s             => show"Option[$s]"
  }

  val fieldGetters = p
    .fields
    .map { field =>
      val allFields = show"""node.fields.getOrElse(${field.name.value.literal}, Nil)"""

      val cases = field.targetTypes.map { tpe =>
        show"""case ${tpe.render}(node) => node"""
      }

      val fieldValue =
        if field.repeated then show"""$allFields.toList.collect {
                                     |${cases.mkString_("\n").indentTrim(2)}
                                     |}""".stripMargin
        else
          show"""$allFields.headOption.map {
                |${cases.mkString_("\n").indentTrim(2)}
                |}""".stripMargin

      show"""def ${field.name.render}: ${renderFieldType(field)} = $fieldValue"""
    }

  val typedChildren = p.children.map { children =>
    val fieldTypeAnnotation = renderChildrenType(children)

    val allChildren = show"""node.children"""

    val cases = children.targetTypes.map { tpe =>
      show"""case ${tpe.render}(node) => node"""
    }

    val fieldValue =
      if children.repeated then show"""$allChildren.toList.collect {
                                      |${cases.mkString_("\n").indentTrim(2)}
                                      |}""".stripMargin
      else
        show"""$allChildren.collectFirst {
              |${cases.mkString_("\n").indentTrim(2)}
              |}""".stripMargin

    show"""def typedChildren: ${fieldTypeAnnotation} = $fieldValue"""
  }

  val typedChildrenPrecise = p
    .children
    .toList
    .flatMap { fieldInfo =>
      fieldInfo.targetTypes.map((fieldInfo.repeated, _)).toList
    }
    .map { (repeated, fieldType) =>
      val fieldTypeAnnotation = renderChildType(fieldType, repeated)
      val childValue =
        if repeated then show"""node.children.toList.collect {
                               |  case ${fieldType.render}(node) => node
                               |}""".stripMargin
        else
          show"""node.children.collectFirst {
                |  case ${fieldType.render}(node) => node
                |}""".stripMargin

      show"""def ${fieldType.asChildName.render}: $fieldTypeAnnotation = $childValue""".stripMargin
    }

  val instanceMethods =
    if (fieldGetters.nonEmpty || typedChildren.nonEmpty || typedChildrenPrecise.nonEmpty) {
      show"""extension (node: $name) {
            |  def select[A](f: $name.Selector => Selection[A]): List[A] = f($name.Selector(List(node))).path
            |  // fields
            |${fieldGetters.mkString_("\n\n").indentTrim(2)}
            |  // typed children
            |${typedChildren.foldMap(_.indentTrim(2)): String}
            |  // precise typed children
            |${typedChildrenPrecise.mkString_("\n\n").indentTrim(2)}
            |}""".stripMargin
    } else
      ""

  val selectorMethods = p
    .fields
    .flatMap {
      case field if field.targetTypes.size == 1 =>
        // format: off
        show"""def ${field.name.render}: ${field.targetTypes.head.render}.Selector = ${field.targetTypes.head.render}.Selector(path.flatMap(_.${field.name.render}))""".stripMargin.some
        // format: on

      case f =>
        System
          .err
          .println(
            s"Skipping selector for field ${f.name} in product $name as it has multiple target types"
          )
        none
    }
    .concat(
      p.children.toList.flatMap(_.targetTypes.toList).map { tpe =>
        // format: off
        show"""def ${tpe.asChildName.render}: ${tpe.render}.Selector = ${tpe.render}.Selector(path.flatMap(_.${tpe.asChildName.render}))""".stripMargin
        // format: on
      }
    )
    .mkString_("\n")

  show"""// Generated code! Do not modify by hand.
        |package playground.generated.nodes
        |
        |import ${classOf[Node].getName()}
        |import playground.treesitter4s.std.Selection
        |
        |opaque type $name <: Node = Node
        |
        |object $name {
        |${instanceMethods.indentTrim(2)}
        |
        |  def apply(node: Node): Either[String, $name] =
        |    if node.tpe == ${p.name.value.literal}
        |    then Right(node)
        |    else Left(s"Expected ${p.name.render}, got $${node.tpe}")
        |
        |  def unsafeApply(node: Node): $name = apply(node).fold(sys.error, identity)
        |
        |  def unapply(node: Node): Option[$name] = apply(node).toOption
        |
        |  final case class Selector(path: List[$name]) extends Selection[$name] {
        |${selectorMethods.indentTrim(4)}
        |
        |    type Self = Selector
        |    protected val remake = Selector.apply
        |  }
        |}
        |""".stripMargin

}

@main def parserGen = {
  val types =
    Json
      .read[NodeTypes](
        Blob(Files.readString(Paths.get("tree-sitter-smithyql/src/node-types.json")))
      )
      .toTry
      .get
      .value

  val base = Paths.get(s"modules/treesitter/src/main/scala/playground/generated/nodes")

  val rendered = types
    .filter(_.named)
    .map(
      // only render field types that are named
      _.focus(_.fields.each.types)
        .modify(_.filter(_.named))
        // don't render the field if it has no types
        .focus(_.fields)
        .modify(_.filter((_, v) => v.types.nonEmpty))
    )
    .fproduct(
      _.render
    )

  Files.createDirectories(base)

  Files.walk(base).iterator().asScala.filter(Files.isRegularFile(_)).foreach(Files.delete)

  rendered
    .foreach { (tpe, code) =>
      Files.writeString(
        base.resolve(s"${tpe.tpe.render}.scala"),
        code,
      )
    }
}

extension (s: String) {

  def indentTrim(n: Int): String = s
    .linesIterator
    .map {
      case line if line.nonEmpty => " " * n + line
      case line                  => line
    }
    .mkString("\n")

  def trimLines: String = s.linesIterator.map(_.stripTrailing()).mkString("\n")

  def literal: String = scala.meta.Lit.String(s).printSyntaxFor(scala.meta.dialects.Scala3)

  def ident: String = {
    // etc.
    val reserved = Set("List", "String", "Boolean", "Null")
    if reserved(s) then s + "_"
    else
      scala.meta.Name(s).printSyntaxFor(scala.meta.dialects.Scala3)
  }

  def fromSnakeCase: String = s.split('_').map(_.capitalize).mkString

}

extension [A](l: List[A]) {

  def requireOnly: A =
    l match {
      case a :: Nil => a
      case _        => throw new IllegalArgumentException(s"Expected exactly one element, got $l")
    }

}
