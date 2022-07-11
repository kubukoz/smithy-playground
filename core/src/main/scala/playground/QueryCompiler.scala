package playground

import cats.Apply
import cats.Id
import cats.data.Ior
import cats.data.IorNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.implicits._
import playground.CompilationErrorDetails._
import playground.smithyql._
import smithy.api.TimestampFormat
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.schema.Alt
import smithy4s.schema.EnumValue
import smithy4s.schema.Primitive
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
import smithy4s.schema.SchemaAlt
import smithy4s.schema.SchemaField
import smithy4s.schema.SchemaVisitor

import util.chaining._
import PartialCompiler.WAST
import smithy4s.schema.CollectionTag
import smithy4s.schema.CollectionTag.IndexedSeqTag
import smithy4s.schema.CollectionTag.ListTag
import smithy4s.schema.CollectionTag.SetTag
import smithy4s.schema.CollectionTag.VectorTag
import smithy4s.capability.EncoderK

trait PartialCompiler[A] {
  final def emap[B](f: A => PartialCompiler.Result[B]): PartialCompiler[B] =
    ast => compile(ast).flatMap(f)

  // TODO: Actually use the powers of Ior. Maybe a custom monad for errors / warnings? Diagnosed[A]? Either+Writer composition?
  def compile(ast: WAST): PartialCompiler.Result[A]
}

object PartialCompiler {
  type Result[+A] = IorNec[CompilationError, A]

  implicit val apply: Apply[PartialCompiler] =
    new Apply[PartialCompiler] {
      def map[A, B](fa: PartialCompiler[A])(f: A => B): PartialCompiler[B] = fa.compile(_).map(f)

      def ap[A, B](ff: PartialCompiler[A => B])(fa: PartialCompiler[A]): PartialCompiler[B] =
        wast => (ff.compile(wast), fa.compile(wast)).parMapN((a, b) => a(b))
    }

  type WAST = WithSource[InputNode[WithSource]]

  val pos: PartialCompiler[SourceRange] = _.range.rightIor
  val unit: PartialCompiler[Unit] = _ => ().rightIor

  def typeCheck[A](
    expected: NodeKind
  )(
    f: PartialFunction[InputNode[WithSource], A]
  ): PartialCompiler[WithSource[A]] =
    ast =>
      ast
        .traverse(f.lift)
        .toRightIor(
          NonEmptyChain(
            CompilationError(
              TypeMismatch(
                expected,
                ast.value.kind,
              ),
              ast.range,
            )
          )
        )

}

final case class CompilationError(err: CompilationErrorDetails, range: SourceRange)

sealed trait CompilationErrorDetails extends Product with Serializable {

  def render: String =
    this match {
      case DuplicateItem => "Duplicate set item"
      case AmbiguousService(matching) =>
        s"""Multiple services are available. Add a use clause to specify the service you want to use.
           |Available services:""".stripMargin + matching
          .map(UseClause(_))
          .map(Formatter.renderUseClause(_).render(Int.MaxValue))
          .mkString_("\n", "\n", "")

      case UnknownService(id, known) =>
        s"Unknown service: ${id.render}. Known services: ${known.map(_.render).mkString(", ")}."

      case RefinementFailure(msg) => s"Refinement failed: $msg."

      case TypeMismatch(expected, actual) => s"Type mismatch: expected $expected, got $actual."

      case UnsupportedNode(tag) => s"Unsupported operation: $tag"

      case OperationNotFound(name, validOperations) =>
        s"Operation ${name.text} not found. Available operations: ${validOperations.map(_.text).mkString_(", ")}."

      case MissingField(label) => s"Missing field $label."

      case InvalidTimestampFormat(expected) => s"Invalid timestamp format, expected $expected."

      case MissingDiscriminator(labels) =>
        s"wrong shape, this union requires one of: ${labels.mkString_(", ")}."

      case EmptyStruct(possibleValues) =>
        s"found empty struct, expected one of: ${possibleValues.mkString_(", ")}."

      case UnknownEnumValue(name, possibleValues) =>
        s"Unknown enum value: $name. Available values: ${possibleValues.mkString(", ")}"

      case StructMismatch(keys, possibleValues) =>
        s"struct mismatch (keys: ${keys.mkString_(", ")}), you must choose exactly one of: ${possibleValues
            .mkString_(", ")}."

      case UnexpectedField(remainingFields) =>
        val expectedRemainingString =
          if (remainingFields.isEmpty)
            ""
          else if (remainingFields.size == 1)
            s" Expected: ${remainingFields.head}."
          else
            s" Expected: one of ${remainingFields.mkString(", ")}."

        s"Unexpected field.$expectedRemainingString"
    }

}

object CompilationErrorDetails {

  val fromResolutionFailure: ResolutionFailure => CompilationErrorDetails = {
    case ResolutionFailure.AmbiguousService(knownServices) =>
      CompilationErrorDetails.AmbiguousService(knownServices)
    case ResolutionFailure.UnknownService(unknownId, knownServices) =>
      CompilationErrorDetails.UnknownService(unknownId, knownServices)

  }

  final case class UnknownService(id: QualifiedIdentifier, knownServices: List[QualifiedIdentifier])
    extends CompilationErrorDetails

  final case class AmbiguousService(
    known: List[QualifiedIdentifier]
  ) extends CompilationErrorDetails

  final case class TypeMismatch(
    expected: NodeKind,
    actual: NodeKind,
  ) extends CompilationErrorDetails

  final case class OperationNotFound(
    name: OperationName,
    validOperations: List[OperationName],
  ) extends CompilationErrorDetails

  final case class MissingField(label: String) extends CompilationErrorDetails

  final case class InvalidTimestampFormat(expected: TimestampFormat) extends CompilationErrorDetails

  final case class MissingDiscriminator(possibleValues: NonEmptyList[String])
    extends CompilationErrorDetails

  final case class EmptyStruct(possibleValues: NonEmptyList[String]) extends CompilationErrorDetails

  final case class UnknownEnumValue(value: String, possibleValues: List[String])
    extends CompilationErrorDetails

  final case class StructMismatch(
    keys: List[String],
    possibleValues: NonEmptyList[String],
  ) extends CompilationErrorDetails

  final case class UnexpectedField(
    remainingFields: List[String]
  ) extends CompilationErrorDetails

  final case class RefinementFailure(msg: String) extends CompilationErrorDetails

  final case class UnsupportedNode(tag: String) extends CompilationErrorDetails

  case object DuplicateItem extends CompilationErrorDetails
}

object QueryCompiler extends SchemaVisitor[PartialCompiler] {

  def primitive[P](shapeId: ShapeId, hints: Hints, tag: Primitive[P]): PartialCompiler[P] =
    tag match {
      case PString => string
      case PBoolean =>
        PartialCompiler
          .typeCheck(NodeKind.Bool) { case b @ BooleanLiteral(_) => b }
          .map(_.value.value)
      case PUnit => _ => ().rightIor
      case PInt =>
        PartialCompiler
          .typeCheck(NodeKind.IntLiteral) { case i @ IntLiteral(_) => i }
          .map(_.value.value)
      case PDocument   => document
      case PShort      => unsupported("short")
      case PBlob       => unsupported("blob")
      case PByte       => unsupported("byte")
      case PBigDecimal => unsupported("bigDecimal")
      case PDouble     => unsupported("double")
      case PBigInt     => unsupported("bigint")
      case PUUID       => unsupported("uuid")
      case PLong       => unsupported("long")
      case PFloat      => unsupported("float")
      case PTimestamp =>
        stringLiteral.emap { s =>
          // todo unhardcode format
          val format = TimestampFormat.DATE_TIME
          Timestamp
            // todo: also, this keeps throwing in an uncatchable way in JS
            .parse(s.value, format)
            .toRightIor(
              CompilationError(
                InvalidTimestampFormat(format),
                s.range,
              )
            )
            .toIorNec
        }
    }

  def collection[C[_], A](
    shapeId: ShapeId,
    hints: Hints,
    tag: CollectionTag[C],
    member: Schema[A],
  ): PartialCompiler[C[A]] =
    tag match {
      case SetTag =>
        val memberToDoc = Document.Encoder.fromSchema(member)

        listWithPos(member.compile(this)).emap { items =>
          items
            .groupBy { case (v, _) => memberToDoc.encode(v) }
            .map(_._2)
            .filter(_.sizeIs > 1)
            .flatMap(_.map(_._2))
            .map(CompilationError(CompilationErrorDetails.DuplicateItem, _))
            .toList
            .pipe(NonEmptyChain.fromSeq(_))
            .toLeftIor(items.map(_._1).toSet)
        }
      case ListTag       => listOf(member)
      case IndexedSeqTag => listOf(member).map(_.toIndexedSeq)
      case VectorTag     => listOf(member).map(_.toVector)
    }

  private def listOf[A](member: Schema[A]) = listWithPos(member.compile(this)).map(_.map(_._1))

  def map[K, V](
    shapeId: ShapeId,
    hints: Hints,
    key: Schema[K],
    value: Schema[V],
  ): PartialCompiler[Map[K, V]] = {
    val fk = key.compile(this)
    val fv = value.compile(this)

    PartialCompiler
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap { struct =>
        val fields = struct.value.fields.value.value

        fields
          .parTraverse { case (k, v) =>
            (
              fk.compile(k.map { key =>
                StringLiteral[Id](key.text).mapK(WithSource.liftId)
              }),
              fv.compile(v),
            ).parTupled
          }
          .map(_.toMap)
      }
  }

  def struct[S](
    shapeId: ShapeId,
    hints: Hints,
    fieldsRaw: Vector[SchemaField[S, _]],
    make: IndexedSeq[Any] => S,
  ): PartialCompiler[S] = {
    val fields = fieldsRaw.map(_.mapK(this))

    val validFields = fields.map(_.label)

    PartialCompiler
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap { struct =>
        // this is a list to keep the original type's ordering
        val remainingValidFields =
          validFields
            .filterNot(
              struct.value.fields.value.keys.map(_.value.text).toSet
            )
            .toList

        val extraFieldErrors: PartialCompiler.Result[Unit] = struct
          .value
          .fields
          .value
          .keys
          .filterNot(field => validFields.contains(field.value.text))
          .map { unexpectedKey =>
            CompilationError(
              UnexpectedField(remainingValidFields),
              unexpectedKey.range,
            )
          }
          .toList
          .toNel
          .map(NonEmptyChain.fromNonEmptyList)
          .toLeftIor(())

        val buildStruct = fields
          .parTraverse { field =>
            val fieldOpt = struct
              .value
              .fields
              .value
              .byName(field.label)(_.value)
              .parTraverse(field.instance.compile)

            if (field.isOptional)
              fieldOpt
            else
              fieldOpt.flatMap {
                _.toRightIor(
                  CompilationError(
                    MissingField(field.label),
                    struct.value.fields.range,
                  )
                ).toIorNec
              }
          }

        buildStruct.map(make) <& extraFieldErrors
      }

  }

  def union[U](
    shapeId: ShapeId,
    hints: Hints,
    alternatives: Vector[Alt[Schema, U, _]],
    dispatcher: Alt.Dispatcher[Schema, U],
  ): PartialCompiler[U] = {
    val alternativesCompiled = alternatives.map(_.mapK(this)).groupBy(_.label).map(_.map(_.head))
    val labels = NonEmptyList.fromListUnsafe(alternatives.toList).map(_.label)

    PartialCompiler
      // todo: should say it's a union
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap {
        case s if s.value.fields.value.size == 1 =>
          val defs = s.value.fields.value
          def go[A](
            alt: Alt[PartialCompiler, U, A]
          ): PartialCompiler[U] = alt.instance.map(alt.inject)

          val (k, v) = defs.head
          val op =
            alternativesCompiled
              .get(k.value.text)
              .toRightIor(
                CompilationError(
                  MissingDiscriminator(labels),
                  s.range,
                )
              )
              .toIorNec

          op.flatMap(go(_).compile(v))

        case s if s.value.fields.value.isEmpty =>
          CompilationError(
            EmptyStruct(labels),
            s.range,
          )
            .leftIor
            .toIorNec

        case s =>
          CompilationError(
            StructMismatch(
              s.value.fields.value.keys.map(_.value.text).toList,
              labels,
            ),
            s.range,
          )
            .leftIor
            .toIorNec
      }
  }

  def biject[A, B](
    schema: Schema[A],
    to: A => B,
    from: B => A,
  ): PartialCompiler[B] = schema.compile(this).map(to)

  def surject[A, B](schema: Schema[A], to: Refinement[A, B], from: B => A): PartialCompiler[B] =
    (schema.compile(this), PartialCompiler.pos).tupled.emap { case (a, pos) =>
      to(a)
        .toIor
        .leftMap { msg =>
          CompilationError(
            CompilationErrorDetails.RefinementFailure(msg),
            pos,
          )
        }
        .toIorNec
    }

  def lazily[A](suspend: Lazy[Schema[A]]): PartialCompiler[A] = {
    val it = suspend.map(_.compile(this))

    it.value.compile(_)
  }

  def unsupported[A](ctx: String): PartialCompiler[A] =
    ast =>
      Ior.leftNec(
        CompilationError(
          UnsupportedNode(ctx),
          ast.range,
        )
      )

  val stringLiteral =
    PartialCompiler.typeCheck(NodeKind.StringLiteral) { case StringLiteral(s) => s }

  val document: PartialCompiler[Document] =
    _.value match {
      case BooleanLiteral(value) => Document.fromBoolean(value).pure[PartialCompiler.Result]
      case IntLiteral(value)     => Document.fromInt(value).pure[PartialCompiler.Result]
      case StringLiteral(value)  => Document.fromString(value).pure[PartialCompiler.Result]
      case Listed(values) => values.value.parTraverse(document.compile(_)).map(Document.array(_))
      case Struct(fields) =>
        fields
          .value
          .value
          .parTraverse { case (key, value) => document.compile(value).tupleLeft(key.value.text) }
          .map(Document.obj(_: _*))
    }

  val string = stringLiteral.map(_.value)

  def enumeration[E](
    shapeId: ShapeId,
    hints: Hints,
    values: List[EnumValue[E]],
    total: E => EnumValue[E],
  ): PartialCompiler[E] = (string, PartialCompiler.pos).tupled.emap { case (name, range) =>
    values
      .find(_.stringValue == name)
      .map(_.value)
      .toRightIor(
        CompilationError(
          UnknownEnumValue(name, values.map(_.stringValue)),
          range,
        )
      )
      .toIorNec
  }

  private def listWithPos[S](
    fs: PartialCompiler[S]
  ): PartialCompiler[List[(S, SourceRange)]] = PartialCompiler
    .typeCheck(NodeKind.Listed) { case l @ Listed(_) => l }
    .emap(
      _.value
        .values
        .value
        .parTraverse { item =>
          (fs, PartialCompiler.pos).tupled.compile(item)
        }
    )

}
