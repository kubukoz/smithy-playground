package playground

import cats.Apply
import cats.data.Ior
import cats.data.IorNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.implicits._
import playground.smithyql._
import smithy4s.schema.Alt
import smithy4s.ByteArray
import smithy4s.schema.Field
import smithy.api.TimestampFormat
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Timestamp
import sourcecode.Enclosing

import java.util.UUID

import PartialCompiler.WAST
import smithy4s.Lazy
import playground.CompilationErrorDetails._
import smithy4s.Refinement

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
      case RefinementFailure(msg)         => s"Refinement failed: $msg."
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
}

class QueryCompilerSchematic extends smithy4s.Schematic[PartialCompiler] {

  def unsupported[A](implicit sc: Enclosing): PartialCompiler[A] =
    ast =>
      Ior.leftNec(
        CompilationError(
          UnsupportedNode(sc.value),
          ast.range,
        )
      )

  def short: PartialCompiler[Short] = unsupported

  val int: PartialCompiler[Int] = PartialCompiler
    .typeCheck(NodeKind.IntLiteral) { case i @ IntLiteral(_) => i }
    .map(_.value.value)

  def long: PartialCompiler[Long] = unsupported

  def double: PartialCompiler[Double] = unsupported

  def float: PartialCompiler[Float] = unsupported

  def bigint: PartialCompiler[BigInt] = unsupported

  def bigdecimal: PartialCompiler[BigDecimal] = unsupported

  val stringLiteral =
    PartialCompiler.typeCheck(NodeKind.StringLiteral) { case StringLiteral(s) => s }

  val string: PartialCompiler[String] = stringLiteral.map(_.value)

  def boolean: PartialCompiler[Boolean] = PartialCompiler
    .typeCheck(NodeKind.Bool) { case b @ BooleanLiteral(_) => b }
    .map(_.value.value)

  def uuid: PartialCompiler[UUID] = unsupported

  def byte: PartialCompiler[Byte] = unsupported

  def bytes: PartialCompiler[ByteArray] = unsupported

  val unit: PartialCompiler[Unit] = PartialCompiler.unit

  def list[S](fs: PartialCompiler[S]): PartialCompiler[List[S]] = PartialCompiler
    .typeCheck(NodeKind.Listed) { case l @ Listed(_) => l }
    .emap(_.value.values.value.parTraverse(fs.compile))

  def set[S](fs: PartialCompiler[S]): PartialCompiler[Set[S]] = unsupported

  def map[K, V](fk: PartialCompiler[K], fv: PartialCompiler[V]): PartialCompiler[Map[K, V]] =
    PartialCompiler
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap { struct =>
        val fields = struct.value.fields.value.value

        fields
          .parTraverse { case (k, v) =>
            (
              fk.compile(k.map { key =>
                StringLiteral[WithSource](key.text)
              }),
              fv.compile(v),
            ).parTupled
          }
          .map(_.toMap)
      }

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

  def union[S](
    first: Alt[PartialCompiler, S, _],
    rest: Vector[Alt[PartialCompiler, S, _]],
  )(
    total: S => Alt.WithValue[PartialCompiler, S, _]
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

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): PartialCompiler[A] = (string, PartialCompiler.pos).tupled.emap { case (name, range) =>
    fromName
      .get(name)
      .toRightIor(
        CompilationError(
          UnknownEnumValue(name, fromName.keys.toList),
          range,
        )
      )
      .toIorNec
  }

  def suspend[A](f: Lazy[PartialCompiler[A]]): PartialCompiler[A] = f.value.compile(_)

  def surjection[A, B](
    f: PartialCompiler[A],
    to: Refinement[A, B],
    from: B => A,
  ): PartialCompiler[B] = (f, PartialCompiler.pos).tupled.emap { case (a, pos) =>
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

  def bijection[A, B](
    f: PartialCompiler[A],
    to: A => B,
    from: B => A,
  ): PartialCompiler[B] = f.map(to)

  val timestamp: PartialCompiler[Timestamp] = stringLiteral.emap { s =>
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

  def withHints[A](fa: PartialCompiler[A], hints: Hints): PartialCompiler[A] = fa // todo

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

}
