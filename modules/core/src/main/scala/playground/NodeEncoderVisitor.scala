package playground

import cats.Contravariant
import cats.Id
import cats.syntax.all.*
import playground.smithyql.Binding
import playground.smithyql.BooleanLiteral
import playground.smithyql.Identifier
import playground.smithyql.InputNode
import playground.smithyql.IntLiteral
import playground.smithyql.Listed
import playground.smithyql.NullLiteral
import playground.smithyql.StringLiteral
import playground.smithyql.Struct
import smithy4s.Bijection
import smithy4s.Document
import smithy4s.Document.DArray
import smithy4s.Document.DBoolean
import smithy4s.Document.DNull
import smithy4s.Document.DNumber
import smithy4s.Document.DObject
import smithy4s.Document.DString
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.ShapeId
import smithy4s.capability.EncoderK
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.CollectionTag.IndexedSeqTag
import smithy4s.schema.CollectionTag.ListTag
import smithy4s.schema.CollectionTag.SetTag
import smithy4s.schema.CollectionTag.VectorTag
import smithy4s.schema.EnumTag
import smithy4s.schema.EnumValue
import smithy4s.schema.Field
import smithy4s.schema.Primitive
import smithy4s.schema.Primitive.*
import smithy4s.schema.Schema
import smithy4s.schema.SchemaVisitor

trait NodeEncoder[A] {

  def toNode(
    a: A
  ): InputNode[Id]

  def transform(
    f: InputNode[Id] => InputNode[Id]
  ): NodeEncoder[A] = toNode.andThen(f).apply(_)

  def listed: NodeEncoder[List[A]] = as => Listed[Id](as.map(this.toNode))

  def atKey(
    key: String
  ): NodeEncoder[A] = transform { result =>
    Struct.one[Id](
      key = Identifier(key),
      value = result,
    )
  }

}

object NodeEncoder {

  implicit val encoderK: EncoderK[NodeEncoder, InputNode[Id]] =
    new EncoderK[NodeEncoder, InputNode[Id]] {

      def apply[A](
        fa: NodeEncoder[A],
        a: A,
      ): InputNode[Id] = fa.toNode(a)

      def absorb[A](
        f: A => InputNode[Id]
      ): NodeEncoder[A] = f(_)

    }

  implicit val catsContravariant: Contravariant[NodeEncoder] =
    new Contravariant[NodeEncoder] {

      def contramap[A, B](
        fa: NodeEncoder[A]
      )(
        f: B => A
      ): NodeEncoder[B] = b => fa.toNode(f(b))

    }

  def derive[A](
    schema: Schema[A]
  ): NodeEncoder[A] = schema.compile(NodeEncoderVisitor)

}

object NodeEncoderVisitor extends SchemaVisitor[NodeEncoder] { self =>

  def primitive[P](
    shapeId: ShapeId,
    hints: Hints,
    tag: Primitive[P],
  ): NodeEncoder[P] =
    tag match {
      case PInt        => int
      case PShort      => short
      case PLong       => long
      case PString     => string
      case PBigInt     => bigint
      case PBoolean    => boolean
      case PBigDecimal => bigdecimal
      case PBlob       => string.contramap(_.toBase64String)
      case PDouble     => double
      case PDocument   => document
      case PFloat      => float
      case PUUID       => string.contramap(_.toString())
      case PByte       => byte
      case PTimestamp  => string.contramap(_.toString)
    }

  def option[A](
    schema: Schema[A]
  ): NodeEncoder[Option[A]] = {
    val base = schema.compile(this)
    val nullDoc = document.toNode(Document.nullDoc)

    _.fold(nullDoc)(base.toNode)
  }

  def collection[C[_], A](
    shapeId: ShapeId,
    hints: Hints,
    tag: CollectionTag[C],
    member: Schema[A],
  ): NodeEncoder[C[A]] =
    tag match {
      case ListTag                            => listOf(member)
      case IndexedSeqTag | SetTag | VectorTag => listOf(member).contramap(_.toList)
    }

  private def listOf[A](
    member: Schema[A]
  ): NodeEncoder[List[A]] = member.compile(this).listed

  def map[K, V](
    shapeId: ShapeId,
    hints: Hints,
    key: Schema[K],
    value: Schema[V],
  ): NodeEncoder[Map[K, V]] = {
    val fk = key.compile(this)
    val fv = value.compile(this)

    _.toList
      .parTraverse { case (k, v) =>
        fk.toNode(k) match {
          case StringLiteral(s) => Binding[Id](Identifier(s), fv.toNode(v)).asRight
          case n                => s"Expected string key, got $n".leftNel
        }
      }
      .map(obj)
      .leftMap(errors =>
        throw new Exception("Map encoding failed: " + errors.toList.mkString(", "))
      )
      .merge
  }

  def enumeration[E](
    shapeId: ShapeId,
    hints: Hints,
    tag: EnumTag[E],
    values: List[EnumValue[E]],
    total: E => EnumValue[E],
  ): NodeEncoder[E] = string.contramap(total(_).name)

  def struct[S](
    shapeId: ShapeId,
    hints: Hints,
    fieldsRaw: Vector[Field[S, _]],
    make: IndexedSeq[Any] => S,
  ): NodeEncoder[S] = {

    def go[A](
      f: Field[S, A]
    ): S => Option[Binding[Id]] = {
      val instance = f.schema.compile(this)

      s =>
        f.getUnlessDefault(s).map(instance.toNode(_)).map { v =>
          Binding[Id](Identifier(f.label), v)
        }
    }

    val fields = fieldsRaw.map(go(_))

    s => obj(fields.mapFilter(_.apply(s)).toList)
  }

  def union[U](
    shapeId: ShapeId,
    hints: Hints,
    alternatives: Vector[Alt[U, _]],
    dispatcher: Alt.Dispatcher[U],
  ): NodeEncoder[U] = dispatcher.compile(new Alt.Precompiler[NodeEncoder] {

    def apply[A](
      label: String,
      instance: Schema[A],
    ): NodeEncoder[A] = instance.compile(self).atKey(label)

  })

  def biject[A, B](
    schema: Schema[A],
    bijection: Bijection[A, B],
  ): NodeEncoder[B] = schema.compile(this).contramap(bijection.from)

  def refine[A, B](
    schema: Schema[A],
    refinement: Refinement[A, B],
  ): NodeEncoder[B] = schema.compile(this).contramap(refinement.from)

  def lazily[A](
    suspend: Lazy[Schema[A]]
  ): NodeEncoder[A] = {
    val mapped = suspend.map(_.compile(this))
    value => mapped.value.toNode(value)
  }

  private val number: NodeEncoder[String] = IntLiteral(_)
  val bigdecimal: NodeEncoder[BigDecimal] = number.contramap(_.toString)
  val bigint: NodeEncoder[BigInt] = number.contramap(_.toString)
  val long: NodeEncoder[Long] = number.contramap(_.toString)
  val int: NodeEncoder[Int] = number.contramap(_.toString)
  val short: NodeEncoder[Short] = number.contramap(_.toString)
  val byte: NodeEncoder[Byte] = number.contramap(_.toString)
  val float: NodeEncoder[Float] = number.contramap(_.toString)
  val double: NodeEncoder[Double] = number.contramap(_.toString)

  val string: NodeEncoder[String] = StringLiteral(_)

  val boolean: NodeEncoder[Boolean] = BooleanLiteral(_)

  private def obj(
    values: List[Binding[Id]]
  ): Struct[Id] = Struct[Id](
    Struct
      .Fields
      .fromSeq[Id](values)
  )

  val document: NodeEncoder[Document] = {
    case DArray(value)   => document.listed.toNode(value.toList)
    case DBoolean(value) => boolean.toNode(value)
    case DNumber(value)  => number.toNode(value.toString())
    case DNull           => NullLiteral()
    case DString(value)  => string.toNode(value)
    case DObject(value) =>
      obj(
        value
          .toList
          .map { case (k, v) => Binding[Id](Identifier(k), document.toNode(v)) }
      )
  }

}
