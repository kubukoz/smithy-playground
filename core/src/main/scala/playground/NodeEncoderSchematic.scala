package playground

import cats.Contravariant
import cats.Id
import cats.implicits._
import cats.tagless.Derive
import playground.smithyql.BooleanLiteral
import playground.smithyql.InputNode
import playground.smithyql.IntLiteral
import playground.smithyql.Listed
import playground.smithyql.StringLiteral
import playground.smithyql.Struct
import schematic.Alt
import schematic.ByteArray
import schematic.Field
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Schematic
import smithy4s.Timestamp
import sourcecode.Enclosing

import java.util.UUID
import smithy4s.Document.DArray
import smithy4s.Document.DBoolean
import smithy4s.Document.DNumber
import smithy4s.Document.DNull
import smithy4s.Document.DString
import smithy4s.Document.DObject

trait NodeEncoder[A] {
  def toNode(a: A): InputNode[Id]
}

object NodeEncoder {
  implicit val contravariant: Contravariant[NodeEncoder] = Derive.contravariant
}

object NodeEncoderSchematic extends Schematic[NodeEncoder] {

  def unsupported[A](implicit sc: Enclosing): NodeEncoder[A] =
    v => throw new Exception(s"Unsupported operation: ${sc.value} for value $v")

  val short: NodeEncoder[Short] = unsupported

  val int: NodeEncoder[Int] = IntLiteral(_)

  val long: NodeEncoder[Long] = l => IntLiteral(l.toInt) // todo this wraps!

  val double: NodeEncoder[Double] = unsupported

  val float: NodeEncoder[Float] = unsupported

  val bigint: NodeEncoder[BigInt] = unsupported

  val bigdecimal: NodeEncoder[BigDecimal] = unsupported

  val string: NodeEncoder[String] = StringLiteral(_)

  val boolean: NodeEncoder[Boolean] = BooleanLiteral(_)

  val uuid: NodeEncoder[UUID] = unsupported

  val byte: NodeEncoder[Byte] = unsupported

  val bytes: NodeEncoder[ByteArray] =
    bytes => StringLiteral(bytes.toString()) // todo this only works for UTF-8 text

  // todo: inconsistent with decoder (takes everything)
  val unit: NodeEncoder[Unit] = struct(Vector())(_ => ())

  def list[S](fs: NodeEncoder[S]): NodeEncoder[List[S]] = elems => Listed[Id](elems.map(fs.toNode))

  def set[S](fs: NodeEncoder[S]): NodeEncoder[Set[S]] = unsupported

  def vector[S](fs: NodeEncoder[S]): NodeEncoder[Vector[S]] = list(fs).contramap(_.toList)

  def map[K, V](fk: NodeEncoder[K], fv: NodeEncoder[V]): NodeEncoder[Map[K, V]] = unsupported

  private def obj(
    values: List[(String, InputNode[Id])]
  ): Struct[Id] = Struct[Id](
    Struct
      .Fields
      .fromSeq[Id](values.map(_.leftMap(Struct.Key(_))))
  )

  def struct[S](
    fields: Vector[Field[NodeEncoder, S, _]]
  )(
    const: Vector[Any] => S
  ): NodeEncoder[S] =
    s => {

      def go[A](
        f: Field[NodeEncoder, S, A]
      ) = f.fold(new Field.Folder[NodeEncoder, S, Option[(String, InputNode[Id])]] {
        def onRequired[F](
          label: String,
          instance: NodeEncoder[F],
          get: S => F,
        ): Option[(String, InputNode[Id])] = Some(label -> instance.toNode(get(s)))

        def onOptional[F](
          label: String,
          instance: NodeEncoder[F],
          get: S => Option[F],
        ): Option[(String, InputNode[Id])] = get(s).map(f => label -> instance.toNode(f))
      })

      obj(fields.flatMap(go(_)).toList)
    }

  def union[S](
    first: Alt[NodeEncoder, S, _],
    rest: Vector[Alt[NodeEncoder, S, _]],
  )(
    total: S => Alt.WithValue[NodeEncoder, S, _]
  ): NodeEncoder[S] =
    s => {
      def go[A](r: Alt.WithValue[NodeEncoder, S, A]) = Struct.one[Id](
        key = Struct.Key(r.alt.label),
        value = r.alt.instance.toNode(r.value),
      )

      go(total(s))
    }

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): NodeEncoder[A] = v => string.toNode(to(v)._1)

  def suspend[A](f: => NodeEncoder[A]): NodeEncoder[A] = unsupported

  def bijection[A, B](
    f: NodeEncoder[A],
    to: A => B,
    from: B => A,
  ): NodeEncoder[B] = b => f.toNode(from(b))

  // todo support formats
  val timestamp: NodeEncoder[Timestamp] = ts => string.toNode(ts.toString())

  // todo
  def withHints[A](fa: NodeEncoder[A], hints: Hints): NodeEncoder[A] = fa

  val document: NodeEncoder[Document] =
    doc =>
      doc match {
        case DArray(value)   => list(document).toNode(value.toList)
        case DBoolean(value) => boolean.toNode(value)
        case DNumber(value) =>
          if (value.isValidInt)
            int.toNode(value.toInt)
          else
            // todo other numbers
            bigdecimal.toNode(value)
        case DNull =>
          // todo nul???
          unsupported[Null].toNode(null)
        case DString(value) => string.toNode(value)
        case DObject(value) => obj(value.toList.map(_.map(document.toNode)))
      }

}
