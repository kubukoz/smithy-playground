package playground

import cats.Functor
import cats.data.NonEmptyList
import cats.implicits._
import playground.smithyql._
import schematic.Alt
import schematic.ByteArray
import schematic.Field
import smithy.api.TimestampFormat
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Timestamp

import java.util.UUID

import util.chaining._

object QueryCompilerSchematic {
  type WAST = AST[WithSource]
}

trait PartialCompiler[A] {
  def compile(ast: AST[WithSource]): A
  // def compile(ast: AST[WithSource]): IorNec[CompilationError, A]
}

object PartialCompiler {

  implicit val functor: Functor[PartialCompiler] =
    new Functor[PartialCompiler] {
      def map[A, B](fa: PartialCompiler[A])(f: A => B): PartialCompiler[B] =
        ast => f(fa.compile(ast))
    }

}

//todo adt
final case class CompilationError(message: String)

class QueryCompilerSchematic
  extends smithy4s.Schematic[PartialCompiler]
  with schematic.struct.GenericAritySchematic[PartialCompiler] {
  def short: PartialCompiler[Short] = ???

  def int: PartialCompiler[Int] = { case IntLiteral(i) => i.value }

  def long: PartialCompiler[Long] = ???

  def double: PartialCompiler[Double] = ???

  def float: PartialCompiler[Float] = ???

  def bigint: PartialCompiler[BigInt] = ???

  def bigdecimal: PartialCompiler[BigDecimal] = ???

  def string: PartialCompiler[String] = { case StringLiteral(s) => s.value }

  def boolean: PartialCompiler[Boolean] = ???

  def uuid: PartialCompiler[UUID] = ???

  def byte: PartialCompiler[Byte] = ???

  def bytes: PartialCompiler[ByteArray] = ???

  def unit: PartialCompiler[Unit] = _ => ()

  def list[S](fs: PartialCompiler[S]): PartialCompiler[List[S]] = ???

  def set[S](fs: PartialCompiler[S]): PartialCompiler[Set[S]] = ???

  def vector[S](fs: PartialCompiler[S]): PartialCompiler[Vector[S]] = ???

  def map[K, V](fk: PartialCompiler[K], fv: PartialCompiler[V]): PartialCompiler[Map[K, V]] = ???

  def genericStruct[S](
    fields: Vector[Field[PartialCompiler, S, _]]
  )(
    const: Vector[Any] => S
  ): PartialCompiler[S] = { case Struct(asts) =>
    const {
      fields.map { field =>
        if (field.isOptional)
          asts.value.value.find(_._1.value == field.label).map(_._2).map(field.instance.compile)
        else
          field.instance.compile(asts.value.value.find(_._1.value == field.label).get._2)
      }
    }
  }

  def union[S](
    first: Alt[PartialCompiler, S, _],
    rest: Vector[Alt[PartialCompiler, S, _]],
  )(
    total: S => Alt.WithValue[PartialCompiler, S, _]
  ): PartialCompiler[S] = {
    val opts = NonEmptyList(first, rest.toList)

    {
      case Struct(defs) if defs.value.value.size == 1 =>
        def go[A](
          alt: Alt[PartialCompiler, S, A]
        ): PartialCompiler[S] = q => alt.instance.compile(q).pipe(alt.inject)

        val (k, v) = defs.value.value.head
        val op = opts
          .find { e =>
            e.label == k.value
          }
          .getOrElse(
            throw new Exception(
              "wrong shape, this union requires one of: " + opts
                .map(_.label)
                .mkString_(", ")
            )
          )

        go(op).compile(v)

      case Struct(m) if m.value.value.isEmpty =>
        throw new Exception(
          "found empty struct, expected one of: " + opts.map(_.label).mkString_(", ")
        )
      case Struct(defs) =>
        throw new Exception(
          s"struct mismatch (keys: ${defs.value.value.keys.map(_.value.text).toList.mkString_(", ")}), you must choose exactly one of: ${opts.map(_.label).mkString_(", ")}"
        )

    }
  }

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): PartialCompiler[A] = ???

  def suspend[A](f: => PartialCompiler[A]): PartialCompiler[A] = ???

  def bijection[A, B](
    f: PartialCompiler[A],
    to: A => B,
    from: B => A,
  ): PartialCompiler[B] = f.map(to)

  def timestamp: PartialCompiler[Timestamp] = { case StringLiteral(s) =>
    Timestamp.parse(s.value, TimestampFormat.DATE_TIME).get /*  */
  }

  def withHints[A](fa: PartialCompiler[A], hints: Hints): PartialCompiler[A] = fa // todo

  def document: PartialCompiler[Document] = ???

}
