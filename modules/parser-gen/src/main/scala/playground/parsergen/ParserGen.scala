package playground.parsergen

import cats.syntax.all.*
import monocle.syntax.all.*
import org.polyvariant.treesitter4s.Node
import smithy4s.Blob
import smithy4s.Document
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

val debug = false

def debugDump(s: String): String =
  if debug then s
  else
    ""

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
    if tpe.subtypes.nonEmpty then renderAdt(tpe)
    else
      renderClass(tpe)

}

def renderAdt(tpe: NodeType) = {
  val name = tpe.tpe.render

  val projections = tpe.subtypes.map { nodeType =>
    // format: off
    show"""def ${nodeType.tpe.renderProjection}: Option[${nodeType.tpe.render}] = ${nodeType.tpe.render}.unapply(node)"""
    // format: on
  }

  show"""// Generated code! Do not modify by hand.
        |package playground.generated.nodes
        |
        |import ${classOf[Node].getName()}
        |
        |opaque type $name <: Node = ${tpe.subtypes.map(_.tpe.render).mkString(" | ")}
        |
        |object $name {
        |
        |  extension (node: $name) {
        |${projections.mkString_("\n").indentTrim(4)}
        |  }
        |
        |  def apply(node: Node): Either[String, $name] = node match {
        |${tpe
         .subtypes
         .map { nodeType =>
           show"""case ${nodeType
               .tpe
               .render}(node) => Right(node)"""
         }
         .mkString_("\n")
         .indentTrim(4)}
        |    case _ => Left(s"Expected $name, got $${node.tpe}")
        |  }
        |
        |  def unapply(node: Node): Option[$name] = apply(node).toOption
        |}
        |/*
        |${debugDump(Json.writeDocumentAsPrettyString(Document.encode(tpe)).trimLines)}
        |*/
        |""".stripMargin
}

def renderClass(tpe: NodeType) = {
  val name = tpe.tpe.render

  val fieldGetters = tpe
    .fields
    .toList
    .map { (k, fieldType) =>
      val typeUnion = fieldType
        .types
        .map(tpe => show"${tpe.tpe.render}")
        .reduceLeftOption(_ + " | " + _)
        .getOrElse(sys.error(s"unexpected empty list of types: $k (in ${tpe.tpe})"))

      val fieldTypeAnnotation = typeUnion.pipe {
        case s if fieldType.multiple => show"List[$s]"
        case s                       => show"Option[$s]"
      }

      val allFields = show"""node.fields.getOrElse(${k.value.literal}, Nil)"""

      val cases = fieldType.types.map { typeInfo =>
        show"""case ${typeInfo.tpe.render}(node) => node"""
      }
      val fieldValue =
        if fieldType.multiple then show"""$allFields.toList.collect {
                                         |${cases.mkString("\n").indentTrim(2)}
                                         |}""".stripMargin
        else
          show"""$allFields.headOption.map {
                |${cases.mkString("\n").indentTrim(2)}
                |}""".stripMargin

      show"""def ${k.render}: ${fieldTypeAnnotation} = $fieldValue"""
    }

  val typedChildren = tpe.children.map { fieldType =>
    val typeUnion = fieldType
      .types
      .map(tpe => show"${tpe.tpe.render}")
      .reduceLeftOption(_ + " | " + _)
      .getOrElse(sys.error(s"unexpected empty list of types in children: (in ${tpe.tpe})"))

    val fieldTypeAnnotation = typeUnion.pipe {
      case s if fieldType.multiple => show"List[$s]"
      case s                       => show"Option[$s]"
    }

    val allChildren = show"""node.children"""

    val cases = fieldType.types.map { typeInfo =>
      show"""case ${typeInfo.tpe.render}(node) => node"""
    }

    val fieldValue =
      if fieldType.multiple then show"""$allChildren.toList.collect {
                                       |${cases.mkString("\n").indentTrim(2)}
                                       |}""".stripMargin
      else
        show"""$allChildren.collectFirst {
              |${cases.mkString("\n").indentTrim(2)}
              |}""".stripMargin

    show"""def typedChildren: ${fieldTypeAnnotation} = $fieldValue"""
  }

  val typedChildrenPrecise = tpe
    .children
    .toList
    .flatMap { fieldInfo =>
      fieldInfo.types.map((fieldInfo.multiple, _))
    }
    .map { (multiple, fieldType) =>
      val fieldTypeAnnotation = fieldType.tpe.render.pipe {
        case s if multiple => show"List[$s]"
        case s             => show"Option[$s]"
      }

      val childValue =
        if multiple then show"""node.children.toList.collect {
                               |  case ${fieldType
                                .tpe
                                .render}(node) => node
                               |}""".stripMargin
        else
          show"""node.children.collectFirst {
                |  case ${fieldType.tpe.render}(node) => node
                |}""".stripMargin

      show"""def ${fieldType
             .tpe
             .asChildName
             .render}: $fieldTypeAnnotation = $childValue""".stripMargin
    }

  val methods =
    if (fieldGetters.nonEmpty || typedChildren.nonEmpty || typedChildrenPrecise.nonEmpty) {
      show"""extension (node: $name) {
            |  // fields
            |${fieldGetters.mkString_("\n\n").indentTrim(2)}
            |  // typed children
            |${typedChildren.foldMap(_.indentTrim(2)): String}
            |  // precise typed children
            |${typedChildrenPrecise.mkString_("\n\n").indentTrim(2)}
            |}""".stripMargin
    } else
      ""

  show"""// Generated code! Do not modify by hand.
        |package playground.generated.nodes
        |
        |import ${classOf[Node].getName()}
        |
        |opaque type $name <: Node = Node
        |
        |object $name {
        |${methods.indentTrim(2)}
        |
        |  def apply(node: Node): Either[String, $name] =
        |    if node.tpe == ${tpe.tpe.value.literal}
        |    then Right(node)
        |    else Left(s"Expected ${tpe.tpe.render}, got $${node.tpe}")
        |  def unsafeApply(node: Node): $name = apply(node).fold(sys.error, identity)
        |  def unapply(node: Node): Option[$name] = apply(node).toOption
        |}
        |
        |/*
        |${debugDump(Json.writeDocumentAsPrettyString(Document.encode(tpe)).trimLines)}
        |*/
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

  val base = Paths.get(s"modules/parser/src/main/scala/playground/generated/nodes")

  Files.walk(base).iterator().asScala.filter(Files.isRegularFile(_)).foreach(Files.delete)

  types
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
