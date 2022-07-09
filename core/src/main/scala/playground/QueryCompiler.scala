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
import smithy4s.Timestamp
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

import util.chaining._
import PartialCompiler.WAST
import smithy4s.schema.EnumValue

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

import smithy4s.~>

object QueryCompiler extends (Schema ~> PartialCompiler) {

  def apply[A](fa: Schema[A]): PartialCompiler[A] =
    fa match {
      case PrimitiveSchema(_, _, tag) =>
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
      case EnumerationSchema(_, _, values, _) => enumeration(values)
      case ListSchema(_, _, member)           => listWithPos(member.compile(this)).map(_.map(_._1))
      case BijectionSchema(underlying, to, _) => underlying.compile(this).map(to)
      case StructSchema(_, _, fields, make)   => struct(fields.map(f => f.mapK(this)))(make)
      case SetSchema(_, _, member) =>
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
      case SurjectionSchema(underlying, refinement, _) =>
        (underlying.compile(this), PartialCompiler.pos).tupled.emap { case (a, pos) =>
          refinement(a)
            .toIor
            .leftMap { msg =>
              CompilationError(
                CompilationErrorDetails.RefinementFailure(msg),
                pos,
              )
            }
            .toIorNec
        }
      case MapSchema(_, _, key, value) => map(key.compile(this), value.compile(this))
      case LazySchema(suspend) =>
        val it = suspend.map(_.compile(this))

        it.value.compile(_)

      case u @ UnionSchema(_, _, _, _) => compileUnion(u)
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
    values: List[EnumValue[E]]
  ) = (string, PartialCompiler.pos).tupled.emap { case (name, range) =>
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

  def struct[S](
    fields: Vector[Field[PartialCompiler, S, _]]
  )(
    const: Vector[Any] => S
  ): PartialCompiler[S] = {
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

        buildStruct.map(const) <& extraFieldErrors
      }

  }

  def map[K, V](fk: PartialCompiler[K], fv: PartialCompiler[V]): PartialCompiler[Map[K, V]] =
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

  def union[S](
    first: Alt[PartialCompiler, S, _],
    rest: Vector[Alt[PartialCompiler, S, _]],
  ): PartialCompiler[S] = {
    val opts = NonEmptyList(first, rest.toList)

    PartialCompiler
      // todo: should say it's a union
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap {
        case s if s.value.fields.value.size == 1 =>
          val defs = s.value.fields.value
          def go[A](
            alt: Alt[PartialCompiler, S, A]
          ): PartialCompiler[S] = alt.instance.map(alt.inject)

          val (k, v) = defs.head
          val op =
            opts
              .find { e =>
                e.label == k.value.text
              }
              .toRightIor(
                CompilationError(
                  MissingDiscriminator(opts.map(_.label)),
                  s.range,
                )
              )
              .toIorNec

          op.flatMap(go(_).compile(v))

        case s if s.value.fields.value.isEmpty =>
          CompilationError(
            EmptyStruct(opts.map(_.label)),
            s.range,
          )
            .leftIor
            .toIorNec

        case s =>
          CompilationError(
            StructMismatch(
              s.value.fields.value.keys.map(_.value.text).toList,
              opts.map(_.label),
            ),
            s.range,
          )
            .leftIor
            .toIorNec
      }
  }

  // Stolen from Schematic code
  private def compileUnion[U](schema: UnionSchema[U]): PartialCompiler[U] = {
    val alts: Vector[Alt[Schema, U, _]] = schema.alternatives
    val head = alts.head
    val tail = alts.tail
    // Pre-compiles the schemas associated to each alternative. This is important
    // because we need to avoid compiling the schemas to codecs upon every dispatch
    val precompiledAlts = (Alt.shiftHintsK[U] andThen Alt.liftK[Schema, PartialCompiler, U](this))
      .unsafeCache(alts.map(smithy4s.Existential.wrap(_)))

    union(precompiledAlts(head), tail.map(precompiledAlts(_)))
  }

}
