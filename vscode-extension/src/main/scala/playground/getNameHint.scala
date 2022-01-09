package playground

import cats.Eval
import schematic.Alt
import schematic.ByteArray
import schematic.Field
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Schematic
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.internals.Hinted

import java.util.UUID

object getNameHint {
  type Result[A] = Hinted[Eval, String]
}

import getNameHint._

class getNameHint extends Schematic[Result] with schematic.struct.GenericAritySchematic[Result] {
  def default[A]: Result[A] = Hinted(Hints(), _ => Eval.later(???))

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

  def genericStruct[S](
    fields: Vector[Field[Result[*], S, _]]
  )(
    const: Vector[Any] => S
  ): Result[S] = Hinted[Eval].from { hints =>
    val hint = hints.get[ShapeId].get

    Eval.now(hint.name)
  }

  def union[S](
    first: Alt[Result, S, _],
    rest: Vector[Alt[Result, S, _]],
  )(
    total: S => Alt.WithValue[Result, S, _]
  ): Result[S] = default

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): Result[A] = default

  def suspend[A](f: => Result[A]): Result[A] = default

  def bijection[A, B](f: Result[A], to: A => B, from: B => A): Result[B] = default

  def timestamp: Result[Timestamp] = default

  def withHints[A](fa: Result[A], hints: Hints): Result[A] = fa.addHints(hints)

  def document: Result[Document] = default

}
