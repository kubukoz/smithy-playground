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
import playground.smithyql._

object QuerySchematic {
  type WAST = AST[WithSource]
}

import QuerySchematic._

class QuerySchematic
  extends smithy4s.Schematic[WAST => *]
  with schematic.struct.GenericAritySchematic[WAST => *] {
  def short: WAST => Short = ???

  def int: WAST => Int = { case IntLiteral(i) => i.value }

  def long: WAST => Long = ???

  def double: WAST => Double = ???

  def float: WAST => Float = ???

  def bigint: WAST => BigInt = ???

  def bigdecimal: WAST => BigDecimal = ???

  def string: WAST => String = { case StringLiteral(s) => s.value }

  def boolean: WAST => Boolean = ???

  def uuid: WAST => UUID = ???

  def byte: WAST => Byte = ???

  def bytes: WAST => ByteArray = ???

  def unit: WAST => Unit = _ => ()

  def list[S](fs: WAST => S): WAST => List[S] = ???

  def set[S](fs: WAST => S): WAST => Set[S] = ???

  def vector[S](fs: WAST => S): WAST => Vector[S] = ???

  def map[K, V](fk: WAST => K, fv: WAST => V): WAST => Map[K, V] = ???

  def genericStruct[S](
    fields: Vector[Field[WAST => *, S, _]]
  )(
    const: Vector[Any] => S
  ): WAST => S = { case Struct(asts) =>
    const {
      fields.map { field =>
        if (field.isOptional)
          asts.value.value.find(_._1.value == field.label).map(_._2).map(field.instance)
        else
          field.instance(asts.value.value.find(_._1.value == field.label).get._2)
      }
    }
  }

  def union[S](
    first: Alt[WAST => *, S, _],
    rest: Vector[Alt[WAST => *, S, _]],
  )(
    total: S => Alt.WithValue[WAST => *, S, _]
  ): WAST => S = {
    val opts = NonEmptyList(first, rest.toList)

    {
      case Struct(defs) if defs.value.value.size == 1 =>
        def go[A](alt: Alt[WAST => *, S, A]): WAST => S = alt.instance.andThen(alt.inject)

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

        go(op)(v)

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
  ): WAST => A = ???

  def suspend[A](f: => WAST => A): WAST => A = ???

  def bijection[A, B](f: WAST => A, to: A => B, from: B => A): WAST => B = f.andThen(to)

  def timestamp: WAST => Timestamp = { case StringLiteral(s) =>
    Timestamp.parse(s.value, TimestampFormat.DATE_TIME).get /*  */
  }

  def withHints[A](fa: WAST => A, hints: Hints): WAST => A = fa // todo

  def document: WAST => Document = ???

}
