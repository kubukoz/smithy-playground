package playground

import cats.data.NonEmptyList
import cats.implicits._
import schematic.Alt
import schematic.ByteArray
import schematic.Field
import smithy.api.TimestampFormat
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Timestamp

import java.util.UUID
import cats.Functor

//todo remove this and use something meaningful in the schematic
case class Identity[A](a: A)

object Identity {

  implicit val functor: Functor[Identity] =
    new Functor[Identity] {
      def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity(f(fa.a))
    }

}

object QuerySchematic {
  // hacks
  type AST = playground.smithyql.AST.high.AST[Identity]
  type IntLiteral = playground.smithyql.AST.high.IntLiteral[Identity]
  val IntLiteral = playground.smithyql.AST.high.IntLiteral
  type Struct = playground.smithyql.AST.high.Struct[Identity]
  val Struct = playground.smithyql.AST.high.Struct
  type StringLiteral = playground.smithyql.AST.high.StringLiteral[Identity]
  val StringLiteral = playground.smithyql.AST.high.StringLiteral
}

import QuerySchematic._

class QuerySchematic
  extends smithy4s.Schematic[AST => *]
  with schematic.struct.GenericAritySchematic[AST => *] {
  def short: AST => Short = ???

  def int: AST => Int = { case IntLiteral(i) => i.a }

  def long: AST => Long = ???

  def double: AST => Double = ???

  def float: AST => Float = ???

  def bigint: AST => BigInt = ???

  def bigdecimal: AST => BigDecimal = ???

  def string: AST => String = { case StringLiteral(s) => s.a }

  def boolean: AST => Boolean = ???

  def uuid: AST => UUID = ???

  def byte: AST => Byte = ???

  def bytes: AST => ByteArray = ???

  def unit: AST => Unit = _ => ()

  def list[S](fs: AST => S): AST => List[S] = ???

  def set[S](fs: AST => S): AST => Set[S] = ???

  def vector[S](fs: AST => S): AST => Vector[S] = ???

  def map[K, V](fk: AST => K, fv: AST => V): AST => Map[K, V] = ???

  def genericStruct[S](
    fields: Vector[Field[AST => *, S, _]]
  )(
    const: Vector[Any] => S
  ): AST => S = { case Struct(asts) =>
    const {
      fields.map { field =>
        if (field.isOptional)
          asts.a.a.get(Identity(field.label)).map(field.instance)
        else
          field.instance(asts.a.a(Identity(field.label)))
      }
    }

  }

  def union[S](
    first: Alt[AST => *, S, _],
    rest: Vector[Alt[AST => *, S, _]],
  )(
    total: S => Alt.WithValue[AST => *, S, _]
  ): AST => S = {
    val opts = NonEmptyList(first, rest.toList)

    {
      case Struct(defs) if defs.a.a.size == 1 =>
        def go[A](alt: Alt[AST => *, S, A]): AST => S = alt.instance.andThen(alt.inject)

        val (k, v) = defs.a.a.head
        val op = opts
          .find { e =>
            e.label == k
          }
          .getOrElse(
            throw new Exception(
              "wrong shape, this union requires one of: " + opts
                .map(_.label)
                .mkString_(", ")
            )
          )

        go(op)(v)

      case Struct(m) if m.a.a.isEmpty =>
        throw new Exception(
          "found empty struct, expected one of: " + opts.map(_.label).mkString_(", ")
        )
      case Struct(defs) =>
        throw new Exception(
          s"struct mismatch (keys: ${defs.a.a.keys.toList.mkString(", ")}), you must choose exactly one of: ${opts.map(_.label).mkString_(", ")}"
        )

    }
  }

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): AST => A = ???

  def suspend[A](f: => AST => A): AST => A = ???

  def bijection[A, B](f: AST => A, to: A => B, from: B => A): AST => B = f.andThen(to)

  def timestamp: AST => Timestamp = { case StringLiteral(s) =>
    Timestamp.parse(s.a, TimestampFormat.DATE_TIME).get /*  */
  }

  def withHints[A](fa: AST => A, hints: Hints): AST => A = fa // todo

  def document: AST => Document = ???

}
