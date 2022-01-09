package playground

import cats.implicits._
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import playground.smithyql.WithSource.StructThing
import schematic.Alt
import schematic.ByteArray
import schematic.Field
import smithy.api.Documentation
import smithy.api.ExternalDocumentation
import smithy.api.Http
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Service
import smithy4s.Timestamp
import typings.vscode.mod

import java.util.UUID

import types._
import util.chaining._

object completions {

  def complete(
    service: Service[Alg, Op]
  ): (mod.TextDocument, mod.Position) => List[mod.CompletionItem] = {
    val completeOperationName = service
      .endpoints
      .map { e =>
        val getName = new getNameHint
        new mod.CompletionItem(
          s"${e.name}: ${e.input.compile(getName).get.value} => ${e.output.compile(getName).get.value}",
          mod.CompletionItemKind.Function,
        ).tap(_.insertText = e.name)
          .tap(
            _.detail = List(
              e.hints.get(Http).map { http =>
                show"HTTP ${http.method.value} ${http.uri.value} "
              },
              e.hints.get(Documentation).map(_.value),
              e.hints.get(ExternalDocumentation).map(_.value).map {
                _.map { case (k, v) => show"""${k.value}: ${v.value}""" }.mkString("\n")
              },
            ).flatten
              .map(_ + " ") // workaround for concatenation in the shortened view
              .mkString("\n\n")
          )
      }

    (doc, pos) =>
      SmithyQLParser.parseFull(doc.getText()) match {
        case Left(_) =>
          // we can try to deal with this later
          Nil
        case Right(q) =>
          val matchingNode = WithSource.atPosition(q)(adapters.fromVscodePosition(doc)(pos))

          matchingNode
            .toList
            .flatMap {
              case WithSource.OperationThing(_) => completeOperationName
              case StructThing(_, ctx) =>
                val e = service.endpoints.find(_.name == q.operationName.value.text).get
                val schem = new CompletionSchematic

                val result = e.input.compile(schem).apply(ctx)

                result.map { key =>
                  new mod.CompletionItem(key, mod.CompletionItemKind.Field)
                    .tap(_.insertText = key + " = ")
                }

              case _ => Nil
            }
      }
  }

}

object CompletionSchematic {
  // from context
  type Result[+A] = List[String] => List[String]
}

class CompletionSchematic
  extends smithy4s.Schematic[CompletionSchematic.Result]
  with schematic.struct.GenericAritySchematic[CompletionSchematic.Result] {
  import CompletionSchematic.Result

  def default: Result[Any] = _ => Nil

  override def genericStruct[S](
    fields: Vector[Field[Result, S, _]]
  )(
    const: Vector[Any] => S
  ): Result[S] = {
    println(fields.map(_.label))

    {
      case Nil       => fields.map(_.label).toList
      case h :: rest => fields.find(_.label == h).toList.flatMap(_.instance(rest))
    }
  }

  def short: Result[Short] = default

  def int: Result[Int] = default

  def long: Result[Long] = default

  def double: Result[Double] = default

  def float: Result[Float] = default

  def bigint: Result[BigInt] = default

  def bigdecimal: Result[BigDecimal] = default

  def string: Result[String] = default

  def boolean: Result[Boolean] = default

  def uuid: Result[UUID] = default

  def byte: Result[Byte] = default

  def bytes: Result[ByteArray] = default

  def unit: Result[Unit] = default

  def list[S](fs: Result[S]): Result[List[S]] = default

  def set[S](fs: Result[S]): Result[Set[S]] = default

  def vector[S](fs: Result[S]): Result[Vector[S]] = default

  def map[K, V](fk: Result[K], fv: Result[V]): Result[Map[K, V]] = default

  def union[S](
    first: Alt[Result, S, _],
    rest: Vector[Alt[Result, S, _]],
  )(
    total: S => Alt.WithValue[Result, S, _]
  ): Result[S] = {
    val all = rest.prepended(first)

    {
      case head :: tail => all.find(_.label == head).toList.flatMap(_.instance(tail))

      case Nil => all.map(_.label).toList
    }

  }

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): Result[A] = default

  def suspend[A](f: => Result[A]): Result[A] = default

  def bijection[A, B](f: Result[A], to: A => B, from: B => A): Result[B] = f

  def timestamp: Result[Timestamp] = default

  def withHints[A](fa: Result[A], hints: Hints): Result[A] = {
    println(hints)
    fa
  }

  def document: Result[Document] = default

}
