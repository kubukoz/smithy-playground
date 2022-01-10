package playground.smithyql

import schematic.Alt
import schematic.ByteArray
import schematic.Field
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Timestamp

import java.util.UUID

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
