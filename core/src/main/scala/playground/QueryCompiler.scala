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
import smithy.api
import smithy.api.TimestampFormat
import smithy4s.Bijection
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.RefinementProvider
import smithy4s.ShapeId
import smithy4s.Surjection
import smithy4s.Timestamp
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.CollectionTag.IndexedSeqTag
import smithy4s.schema.CollectionTag.ListTag
import smithy4s.schema.CollectionTag.SetTag
import smithy4s.schema.CollectionTag.VectorTag
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
import smithy4s.schema.Schema.BijectionSchema
import smithy4s.schema.Schema.CollectionSchema
import smithy4s.schema.Schema.EnumerationSchema
import smithy4s.schema.Schema.LazySchema
import smithy4s.schema.Schema.MapSchema
import smithy4s.schema.Schema.PrimitiveSchema
import smithy4s.schema.Schema.RefinementSchema
import smithy4s.schema.Schema.StructSchema
import smithy4s.schema.Schema.UnionSchema
import smithy4s.schema.SchemaField
import smithy4s.schema.SchemaVisitor
import smithy4s.~>

import java.util.UUID

import util.chaining._
import PartialCompiler.WAST

trait PartialCompiler[A] {
  final def emap[B](f: A => PartialCompiler.Result[B]): PartialCompiler[B] =
    ast => compile(ast).flatMap(f)

  // TODO: Actually use the powers of Ior. Maybe a custom monad for errors / warnings? Diagnosed[A]? Either+Writer composition?
  def compile(ast: WAST): PartialCompiler.Result[A]

  /** Makes all error-level diagnostics fatal on the top level of this compiler instance.
    */
  def seal: PartialCompiler[A] =
    ast =>
      compile(ast).fold(
        Ior.left(_),
        Ior.right(_),
        (e, a) =>
          if (e.exists(_.isError))
            Ior.left(e)
          else
            Ior.both(e, a),
      )

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
            CompilationError.error(
              TypeMismatch(
                expected,
                ast.value.kind,
              ),
              ast.range,
            )
          )
        )

}

sealed trait DiagnosticSeverity extends Product with Serializable

object DiagnosticSeverity {
  case object Warning extends DiagnosticSeverity
  case object Error extends DiagnosticSeverity
  case object Information extends DiagnosticSeverity
}

sealed trait DiagnosticTag extends Product with Serializable

object DiagnosticTag {
  case object Deprecated extends DiagnosticTag
  case object Unused extends DiagnosticTag
}

final case class CompilationError(
  err: CompilationErrorDetails,
  range: SourceRange,
  severity: DiagnosticSeverity,
  tags: Set[DiagnosticTag],
) {
  def deprecated: CompilationError = copy(tags = tags + DiagnosticTag.Deprecated)

  def isError: Boolean = severity == DiagnosticSeverity.Error
  def isWarning: Boolean = severity == DiagnosticSeverity.Warning
}

object CompilationError {

  def error(
    err: CompilationErrorDetails,
    range: SourceRange,
  ): CompilationError = default(err, range, DiagnosticSeverity.Error)

  def warning(
    err: CompilationErrorDetails,
    range: SourceRange,
  ): CompilationError = default(err, range, DiagnosticSeverity.Warning)

  def default(
    err: CompilationErrorDetails,
    range: SourceRange,
    severity: DiagnosticSeverity,
  ): CompilationError = CompilationError(
    err = err,
    range = range,
    severity = severity,
    tags = Set.empty,
  )

}

sealed trait CompilationErrorDetails extends Product with Serializable {

  def render: String =
    this match {
      case Message(text) => text
      case DeprecatedMember(info) =>
        s"Deprecated union member${CompletionItem.deprecationString(info)}"
      case DeprecatedField(info) => s"Deprecated field${CompletionItem.deprecationString(info)}"
      case InvalidUUID           => "Invalid UUID"
      case EnumFallback(enumName) =>
        s"""Matching enums by value is deprecated and may be removed in the future. Use $enumName instead.""".stripMargin
      case DuplicateItem => "Duplicate item - some entries will be dropped to fit in a set shape."
      case AmbiguousService(matching) =>
        s"""Multiple services are available. Add a use clause to specify the service you want to use.
           |Available services:""".stripMargin + matching
          .map(UseClause(_))
          .map(Formatter.renderUseClause(_).render(Int.MaxValue))
          .mkString_("\n", "\n", ".")

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

  // todo: remove
  final case class Message(text: String) extends CompilationErrorDetails
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

  case object InvalidUUID extends CompilationErrorDetails

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
  case class DeprecatedField(info: api.Deprecated) extends CompilationErrorDetails
  case class DeprecatedMember(info: api.Deprecated) extends CompilationErrorDetails

  final case class EnumFallback(enumName: String) extends CompilationErrorDetails
}

object QueryCompiler {
  val full = new TransitiveCompiler(AddDynamicRefinements) andThen QueryCompilerInternal

}

// Applies the underlying transformation on each node of the schema that has its own hints
class TransitiveCompiler(underlying: Schema ~> Schema) extends (Schema ~> Schema) {

  def apply[A](fa: Schema[A]): Schema[A] =
    fa match {
      case e @ EnumerationSchema(_, _, _, _) => underlying(e)
      case p @ PrimitiveSchema(_, _, _)      => underlying(p)
      case u @ UnionSchema(_, _, _, _) =>
        underlying(u.copy(alternatives = u.alternatives.map(_.mapK(this))))
      case BijectionSchema(s, bijection) => BijectionSchema(this(s), bijection)
      case LazySchema(suspend)           => LazySchema(suspend.map(this.apply))
      case RefinementSchema(underlying, refinement) =>
        RefinementSchema(this(underlying), refinement)
      case c @ CollectionSchema(_, _, _, _) => c.copy(member = this(c.member))
      case m @ MapSchema(_, _, _, _) => underlying(m.copy(key = this(m.key), value = this(m.value)))
      case s @ StructSchema(_, _, _, _) => underlying(s.copy(fields = s.fields.map(_.mapK(this))))
    }

}

object AddDynamicRefinements extends (Schema ~> Schema) {

  // todo: https://github.com/disneystreaming/smithy4s/pull/348
  private def void[C, A](
    underlying: RefinementProvider[C, A, _]
  ): RefinementProvider.Simple[C, A] =
    Refinement
      .drivenBy[C]
      .contextual[A, A](c => Surjection(v => underlying.make(c).apply(v).as(v), identity))(
        underlying.tag
      )

  private implicit class SchemaOps[A](schema: Schema[A]) {
    // could this live in smithy4s?
    def reifyHint[B](implicit rp: RefinementProvider[B, A, _]): Schema[A] =
      schema.hints.get(rp.tag).fold(schema)(schema.validated(_)(void(rp)))
  }

  private def collection[C[_], A](
    schema: Schema.CollectionSchema[C, A]
  ): Schema[C[A]] =
    schema.tag match {
      case ListTag   => schema.reifyHint(RefinementProvider.iterableLengthConstraint[List, A])
      case VectorTag => schema.reifyHint(RefinementProvider.iterableLengthConstraint[Vector, A])
      case SetTag    => schema.reifyHint(RefinementProvider.iterableLengthConstraint[Set, A])
      case IndexedSeqTag =>
        schema.reifyHint(RefinementProvider.iterableLengthConstraint[IndexedSeq, A])
    }

  def apply[A](schema: Schema[A]): Schema[A] =
    schema match {
      case PrimitiveSchema(_, _, tag) =>
        tag match {
          case PString => schema.reifyHint[api.Length].reifyHint[api.Pattern]
          case PInt    => schema.reifyHint[api.Range]
          case _       => schema
        }

      case c: CollectionSchema[_, _] => collection(c)
      case m: MapSchema[_, _]        => m.reifyHint[api.Length]
      case _                         => schema
    }

}

object QueryCompilerInternal extends SchemaVisitor[PartialCompiler] {

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
      case PUUID =>
        stringLiteral.emap { s =>
          Either
            .catchOnly[IllegalArgumentException](UUID.fromString(s.value))
            .toIor
            .leftMap(_ => CompilationError.error(InvalidUUID, s.range))
            .toIorNec
        }

      case PLong  => unsupported("long")
      case PFloat => unsupported("float")
      case PTimestamp =>
        stringLiteral.emap { s =>
          // We don't support other formats for the simple reason that it's not necessary:
          // this is just like multiple union encodings - it doesn't matter how you write your queries,
          // the actual serialization format will be used by the client when we eventually use it in the Runner.
          val format = TimestampFormat.DATE_TIME

          Timestamp
            .parse(s.value, format)
            .toRightIor(
              CompilationError.error(
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
          val success = items.map(_._1).toSet

          val duplications = items
            .groupBy { case (v, _) => memberToDoc.encode(v) }
            .map(_._2)
            .filter(_.sizeIs > 1)
            .flatMap(_.map(_._2))
            // todo: reorganize this so it only shows the warning once with extra locations (which ideally would be marked as unused, but idk if possible)
            .map(CompilationError.warning(CompilationErrorDetails.DuplicateItem, _))
            .toList
            .pipe(NonEmptyChain.fromSeq(_))

          duplications match {
            case None         => success.rightIor
            case Some(errors) => Ior.both(errors, success)
          }
        }

      case ListTag       => listOf(member)
      case IndexedSeqTag => listOf(member).map(_.toIndexedSeq)
      case VectorTag     => listOf(member).map(_.toVector)
    }

  private def listOf[A](
    member: Schema[A]
  ) = listWithPos(member.compile(this))
    .map(_.map(_._1))

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
    val deprecatedFields =
      fieldsRaw.flatMap { f =>
        f.hints.get(api.Deprecated).tupleLeft(f.label)
      }.toMap

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

        val presentKeys = struct.value.fields.value.keys

        val extraFieldErrors: PartialCompiler.Result[Unit] = presentKeys
          .filterNot(field => validFields.contains(field.value.text))
          .map { unexpectedKey =>
            CompilationError.error(
              UnexpectedField(remainingValidFields),
              unexpectedKey.range,
            )
          }
          .toList
          .toNel
          .map(NonEmptyChain.fromNonEmptyList)
          .toLeftIor(())
          .combine(Ior.right(()))

        val deprecatedFieldWarnings: PartialCompiler.Result[Unit] = presentKeys
          .flatMap { key =>
            deprecatedFields.get(key.value.text).map { info =>
              CompilationError
                .warning(CompilationErrorDetails.DeprecatedField(info), key.range)
                .deprecated
            }
          }
          .toList
          .toNel
          .map(NonEmptyChain.fromNonEmptyList)
          .toLeftIor(())
          .combine(Ior.right(()))

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
                  CompilationError.error(
                    MissingField(field.label),
                    struct.value.fields.range,
                  )
                ).toIorNec
              }
          }

        buildStruct.map(make) <& extraFieldErrors <& deprecatedFieldWarnings
      }

  }

  def union[U](
    shapeId: ShapeId,
    hints: Hints,
    alternatives: Vector[Alt[Schema, U, _]],
    dispatcher: Alt.Dispatcher[Schema, U],
  ): PartialCompiler[U] = {
    val alternativesCompiled = alternatives.map(_.mapK(this)).groupBy(_.label).map(_.map(_.head))
    val deprecatedAlternativeLabels =
      alternatives.flatMap(alt => alt.hints.get(api.Deprecated).tupleLeft(alt.label)).toMap

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
                CompilationError.error(
                  MissingDiscriminator(labels),
                  s.range,
                )
              )
              .toIorNec

          val deprecationWarning: PartialCompiler.Result[Unit] = deprecatedAlternativeLabels
            .get(k.value.text)
            .map { info =>
              CompilationError
                .warning(CompilationErrorDetails.DeprecatedMember(info), k.range)
                .deprecated
            }
            .toList
            .toNel
            .map(NonEmptyChain.fromNonEmptyList)
            .toLeftIor(())
            .combine(Ior.right(()))

          op.flatMap(go(_).compile(v)) <& deprecationWarning

        case s if s.value.fields.value.isEmpty =>
          CompilationError
            .error(
              EmptyStruct(labels),
              s.range,
            )
            .leftIor
            .toIorNec

        case s =>
          CompilationError
            .error(
              StructMismatch(
                s.value.fields.value.keys.map(_.value.text),
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
    bijection: Bijection[A, B],
  ): PartialCompiler[B] = schema.compile(this).map(bijection.apply)

  def refine[A, B](
    schema: Schema[A],
    refinement: Refinement[A, B],
  ): PartialCompiler[B] = surject(schema.compile(this), refinement)

  private def surject[A, B, C](
    pc: PartialCompiler[A],
    refinement: Refinement[A, B],
  ): PartialCompiler[B] = (pc, PartialCompiler.pos).tupled.emap { case (a, pos) =>
    refinement(a)
      .toIor
      .leftMap { msg =>
        CompilationError.error(
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
        CompilationError.error(
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
      // parTraverse in this file isn't going to work like you think it will
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
    val byValue = values
      .find(_.stringValue == name)

    val byName = values
      .find(_.name == name)

    (byName, byValue) match {
      case (Some(v), _) => v.value.pure[PartialCompiler.Result]

      case (None, Some(v)) =>
        Ior.bothNec(CompilationError.warning(EnumFallback(v.name), range).deprecated, v.value)

      case (None, None) =>
        Ior.leftNec(CompilationError.error(UnknownEnumValue(name, values.map(_.name)), range))
    }

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
