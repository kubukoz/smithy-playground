package playground

import cats.Contravariant
import cats.Id
import cats.implicits._
import playground.smithyql.BooleanLiteral
import playground.smithyql.InputNode
import playground.smithyql.IntLiteral
import playground.smithyql.Listed
import playground.smithyql.StringLiteral
import playground.smithyql.Struct
import smithy4s.Document
import smithy4s.Document.DArray
import smithy4s.Document.DBoolean
import smithy4s.Document.DNull
import smithy4s.Document.DNumber
import smithy4s.Document.DObject
import smithy4s.Document.DString
import smithy4s.schema.Alt
import smithy4s.schema.Field
import smithy4s.schema.Primitive.PBigDecimal
import smithy4s.schema.Primitive.PBigInt
import smithy4s.schema.Primitive.PBlob
import smithy4s.schema.Primitive.PBoolean
import smithy4s.schema.Primitive.PByte
import smithy4s.schema.Primitive.PDocument
import smithy4s.schema.Primitive.PDouble
import smithy4s.schema.Primitive.PFloat
import smithy4s.schema.Primitive.PInt
import smithy4s.schema.Primitive.PLong
import smithy4s.schema.Primitive.PShort
import smithy4s.schema.Primitive.PString
import smithy4s.schema.Primitive.PTimestamp
import smithy4s.schema.Primitive.PUUID
import smithy4s.schema.Primitive.PUnit
import smithy4s.schema.Schema
import smithy4s.schema.Schema.BijectionSchema
import smithy4s.schema.Schema.EnumerationSchema
import smithy4s.schema.Schema.LazySchema
import smithy4s.schema.Schema.ListSchema
import smithy4s.schema.Schema.MapSchema
import smithy4s.schema.Schema.PrimitiveSchema
import smithy4s.schema.Schema.SetSchema
import smithy4s.schema.Schema.StructSchema
import smithy4s.schema.Schema.SurjectionSchema
import smithy4s.schema.Schema.UnionSchema
import smithy4s.~>

trait NodeEncoder[A] {
  def toNode(a: A): InputNode[Id]
}

object NodeEncoder {

  implicit val contravariant: Contravariant[NodeEncoder] =
    new Contravariant[NodeEncoder] {
      def contramap[A, B](fa: NodeEncoder[A])(f: B => A): NodeEncoder[B] = b => fa.toNode(f(b))
    }

  def derive[A](schema: Schema[A]): NodeEncoder[A] = schema.compile(NodeEncoderVisitor)

}

object NodeEncoderVisitor extends (Schema ~> NodeEncoder) {

  def apply[A](fa: Schema[A]): NodeEncoder[A] =
    fa match {
      case LazySchema(suspend) =>
        val mapped = suspend.map(_.compile(this))
        value => mapped.value.toNode(value)

      case SurjectionSchema(underlying, _, from) => underlying.compile(this).contramap(from)
      case BijectionSchema(underlying, _, from)  => underlying.compile(this).contramap(from)
      case EnumerationSchema(_, _, _, total)     => string.contramap(total(_).stringValue)
      case UnionSchema(_, _, alternatives, dispatch) =>
        val altsCompiled = alternatives.map(_.mapK(this))

        s => {
          def go[X](r: Alt.WithValue[NodeEncoder, A, X]) = Struct.one[Id](
            key = Struct.Key(r.alt.label),
            value = r.alt.instance.toNode(r.value),
          )

          go {
            dispatch
              .andThen { result =>
                result.copy[NodeEncoder, A, Any](alt =
                  altsCompiled
                    .find(_.label == result.alt.label)
                    .get
                    .asInstanceOf[Alt[NodeEncoder, A, Any]]
                )
              }
              .apply(s)
          }
        }
      case MapSchema(_, _, key, value)   => map(key.compile(this), value.compile(this))
      case ListSchema(_, _, member)      => list(member.compile(this))
      case SetSchema(_, _, member)       => list(member.compile(this)).contramap(_.toList)
      case StructSchema(_, _, fields, _) => struct(fields.map(_.mapK(this)))
      case PrimitiveSchema(_, _, tag) =>
        tag match {
          case PInt        => int
          case PShort      => unsupported("short")
          case PLong       => int.contramap(_.toInt) // todo: wraps
          case PString     => string
          case PBigInt     => unsupported("bigint")
          case PBoolean    => boolean
          case PBigDecimal => bigdecimal
          case PBlob       => string.contramap(_.toString) // todo this only works for UTF-8 text
          case PDouble     => unsupported("double")
          case PDocument   => document
          case PFloat      => unsupported("float")
          case PUnit       =>
            // todo: inconsistent with decoder (takes everything)
            struct(Vector())
          case PUUID      => unsupported("uuid")
          case PByte      => unsupported("byte")
          case PTimestamp =>
            // todo support formats
            string.contramap(_.toString)
        }
    }

  def unsupported[A](tag: String): NodeEncoder[A] =
    v => throw new Exception(s"Unsupported operation: $tag for value $v")

  val bigdecimal: NodeEncoder[BigDecimal] = unsupported("bigdecimal")
  val int: NodeEncoder[Int] = IntLiteral(_)

  val string: NodeEncoder[String] = StringLiteral(_)

  val boolean: NodeEncoder[Boolean] = BooleanLiteral(_)

  def list[S](fs: NodeEncoder[S]): NodeEncoder[List[S]] = elems => Listed[Id](elems.map(fs.toNode))

  def set[S](fs: NodeEncoder[S]): NodeEncoder[Set[S]] = list(fs).contramap(_.toList)

  def map[K, V](fk: NodeEncoder[K], fv: NodeEncoder[V]): NodeEncoder[Map[K, V]] =
    _.toList
      .parTraverse { case (k, v) =>
        fk.toNode(k) match {
          case StringLiteral(s) => (s -> fv.toNode(v)).asRight
          case n                => s"Expected string key, got $n".leftNel
        }
      }
      .map(obj)
      .leftMap(errors =>
        throw new Exception("Map encoding failed: " + errors.toList.mkString(", "))
      )
      .merge

  private def obj(
    values: List[(String, InputNode[Id])]
  ): Struct[Id] = Struct[Id](
    Struct
      .Fields
      .fromSeq[Id](values.map(_.leftMap(Struct.Key(_))))
  )

  def struct[S](
    fields: Vector[Field[NodeEncoder, S, _]]
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
    total: S => Alt.WithValue[NodeEncoder, S, _]
  ): NodeEncoder[S] =
    s => {
      def go[A](r: Alt.WithValue[NodeEncoder, S, A]) = Struct.one[Id](
        key = Struct.Key(r.alt.label),
        value = r.alt.instance.toNode(r.value),
      )

      go(total(s))
    }

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
          unsupported[Null]("null").toNode(null)
        case DString(value) => string.toNode(value)
        case DObject(value) => obj(value.toList.map(_.map(document.toNode)))
      }

}
