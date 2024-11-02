package playground.parsergen

import cats.syntax.all.*
import monocle.syntax.all.*
import org.polyvariant.treesitter4s.Node
import smithy4s.Blob
import smithy4s.Document
import smithy4s.json.Json
import treesittersmithy.NodeType
import treesittersmithy.NodeTypes
import treesittersmithy.TypeName
import util.chaining.*

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.meta.Dialect

val debug = false

def debugDump(s: String): String =
  if debug then s
  else
    ""

extension (tn: TypeName) {
  def render: String = tn.value.smartCapitalize.ident
  def asEnumCase: TypeName = TypeName(tn.value + "Case")
}

extension (tpe: NodeType) {

  def render: String =
    if tpe.subtypes.nonEmpty then renderAdt(tpe)
    else
      renderClass(tpe)

}

def renderAdt(tpe: NodeType) = {
  val name = tpe.tpe.render

  val enumCases = tpe.subtypes.map { nodeType =>
    show"""case ${nodeType.tpe.asEnumCase.render}(value: ${nodeType.tpe.render})"""
  }

  show"""// Generated code! Do not modify by hand.
        |package playground.generated.nodes
        |
        |import ${classOf[Node].getName()}
        |
        |enum $name {
        |${enumCases.mkString_("\n").indentTrim(2)}
        |
        |  def asNode: Node = this match {
        |${tpe
         .subtypes
         .map { nodeType =>
           show"""case ${nodeType.tpe.asEnumCase.render}(value) => value.node"""
         }
         .mkString_("\n")
         .indentTrim(4)}
        |  }
        |}
        |
        |object $name {
        |  def apply(node: Node): $name = node match {
        |${tpe
         .subtypes
         .map { nodeType =>
           show"""case node @ ${nodeType
               .tpe
               .render}() => ${nodeType.tpe.asEnumCase.render}(${nodeType.tpe.render}(node))"""
         }
         .mkString_("\n")
         .indentTrim(4)}
        |  }
        |}
        |
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
      val singleFieldType = fieldType
        .types
        .map(tpe => show"${tpe.tpe.render}")
        .reduceLeft(_ + " | " + _)

      val fieldTypeAnnotation = singleFieldType.pipe {
        case s if fieldType.multiple => show"List[$s]"
        case s                       => s
      }

      val allFields = show"""node.fields(${k.value.literal})"""

      val cases = fieldType.types.map { typeInfo =>
        show"""case node @ ${typeInfo.tpe.render}() => ${typeInfo.tpe.render}(node)"""
      }
      val fieldValue =
        if fieldType.multiple then show"""$allFields.toList.collect {
                                         |${cases.mkString("\n").indentTrim(2)}
                                         |}""".stripMargin
        else
          show"""${singleFieldType}($allFields.head)"""

      show"""def ${k.value}: ${fieldTypeAnnotation} = $fieldValue"""
    }

  show"""// Generated code! Do not modify by hand.
        |package playground.generated.nodes
        |
        |import ${classOf[Node].getName()}
        |
        |case class $name /* private */(node: Node) extends Node {

        |${fieldGetters
         .mkString_("\n")
         .indentTrim(2)}
        |
        |  export node.*
        |}
        |
        |object $name {
        |  def unapply(node: Node): scala.Boolean = node.tpe == ${tpe.tpe.value.literal}
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
    .map(_.focus(_.fields.each.types).modify(_.filter(_.named)))
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

  def smartCapitalize: String = {
    val (before, after) = s.span(!_.isLetter)
    before + after.capitalize
  }

}

extension [A](l: List[A]) {

  def requireOnly: A =
    l match {
      case a :: Nil => a
      case _        => throw new IllegalArgumentException(s"Expected exactly one element, got $l")
    }

}
