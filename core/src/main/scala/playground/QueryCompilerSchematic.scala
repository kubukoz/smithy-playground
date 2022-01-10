package playground

import cats.Functor
import cats.data.IorNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.implicits._
import cats.tagless.Derive
import playground.smithyql._
import schematic.Alt
import schematic.ByteArray
import schematic.Field
import smithy.api.TimestampFormat
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Timestamp

import java.util.UUID
import PartialCompiler.WAST
import cats.data.Ior
import sourcecode.Enclosing
import cats.Apply

trait PartialCompiler[A] {
  final def emap[B](f: A => PartialCompiler.Result[B]): PartialCompiler[B] =
    ast => compile(ast).flatMap(f)

  // TODO: Actually use the powers of Ior. Maybe a custom monad for errors / warnings? Diagnosed[A]? Either+Writer composition?
  def compile(ast: WAST): PartialCompiler.Result[A]
}

object PartialCompiler {
  type Result[+A] = IorNec[CompilationError, A]

  implicit val functor: Apply[PartialCompiler] = Derive.apply

  type WAST = WithSource[InputNode[WithSource]]

  val pos: PartialCompiler[SourceRange] = _.range.rightIor
  val unit: PartialCompiler[Unit] = _ => ().rightIor

  def fromPF[A](
    f: PartialFunction[InputNode[WithSource], A]
  )(
    orElseMessage: WAST => String
  ): PartialCompiler[WithSource[A]] =
    ast =>
      f.lift(ast.value)
        .toRightIor(
          NonEmptyChain(
            CompilationError(
              orElseMessage(ast),
              Some(ast.range),
            )
          )
        )
        .map(a => ast.copy(value = a))

}

//todo adt
final case class CompilationError(message: String, range: Option[SourceRange] = None)

class QueryCompilerSchematic
  extends smithy4s.Schematic[PartialCompiler]
  with schematic.struct.GenericAritySchematic[PartialCompiler] {

  def todo[A](implicit sc: Enclosing): PartialCompiler[A] =
    _ => Ior.leftNec(CompilationError(s"Unsupported operation: ${sc.value}"))

  def short: PartialCompiler[Short] = todo

  val int: PartialCompiler[Int] = PartialCompiler
    .fromPF { case i @ IntLiteral(_) => i }(ast =>
      s"Type mismatch: expected ${NodeKind.IntLiteral}, this is a ${ast.value.kind}"
    )
    .map(_.value.value)

  def long: PartialCompiler[Long] = todo

  def double: PartialCompiler[Double] = todo

  def float: PartialCompiler[Float] = todo

  def bigint: PartialCompiler[BigInt] = todo

  def bigdecimal: PartialCompiler[BigDecimal] = todo

  val stringLiteral =
    PartialCompiler.fromPF { case StringLiteral(s) => s }(ast =>
      s"Expected ${NodeKind.StringLiteral}, got ${ast.value.kind} instead"
    )

  val string: PartialCompiler[String] = stringLiteral.map(_.value)

  def boolean: PartialCompiler[Boolean] = todo

  def uuid: PartialCompiler[UUID] = todo

  def byte: PartialCompiler[Byte] = todo

  def bytes: PartialCompiler[ByteArray] = todo

  val unit: PartialCompiler[Unit] = PartialCompiler.unit

  def list[S](fs: PartialCompiler[S]): PartialCompiler[List[S]] = todo

  def set[S](fs: PartialCompiler[S]): PartialCompiler[Set[S]] = todo

  def vector[S](fs: PartialCompiler[S]): PartialCompiler[Vector[S]] = todo

  def map[K, V](fk: PartialCompiler[K], fv: PartialCompiler[V]): PartialCompiler[Map[K, V]] = todo

  def genericStruct[S](
    fields: Vector[Field[PartialCompiler, S, _]]
  )(
    const: Vector[Any] => S
  ): PartialCompiler[S] = {
    val validFields = fields.map(_.label).toSet

    PartialCompiler
      .fromPF { case s @ Struct(_) => s }(ast =>
        s"Expected ${NodeKind.Struct}, got ${ast.value.kind} instead"
      )
      .emap { struct =>
        val remainingValidFields = validFields -- struct.value.fields.value.keys.map(_.value.text)
        val expectedRemainingString =
          if (remainingValidFields.isEmpty)
            ""
          else if (remainingValidFields.size == 1)
            s". Expected: ${remainingValidFields.head}"
          else
            s". Expected: one of ${remainingValidFields.mkString(", ")}"

        val extraFieldErrors: PartialCompiler.Result[Unit] = struct
          .value
          .fields
          .value
          .keys
          .filterNot(validFields.compose(_.value.text))
          .map { unexpectedKey =>
            CompilationError(
              "Unexpected field" + expectedRemainingString,
              Some(unexpectedKey.range),
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
              .find(_._1.value.text == field.label)
              .map(_._2)
              .parTraverse(field.instance.compile)

            if (field.isOptional)
              fieldOpt
            else
              fieldOpt.flatMap {
                _.toRightIor(
                  CompilationError(s"Missing field ${field.label}", struct.value.fields.range.some)
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
      .fromPF { case s @ Struct(_) => s }(ast =>
        s"Expected a union struct, got ${ast.value.kind} instead"
      )
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
                  "wrong shape, this union requires one of: " + opts
                    .map(_.label)
                    .mkString_(", ")
                )
              )
              .toIorNec

          op.flatMap(go(_).compile(v))

        case s if s.value.fields.value.isEmpty =>
          CompilationError(
            "found empty struct, expected one of: " + opts.map(_.label).mkString_(", ")
          ).leftIor.toIorNec

        case s =>
          CompilationError(
            s"struct mismatch (keys: ${s.value.fields.value.keys.map(_.value.text).toList.mkString_(", ")}), you must choose exactly one of: ${opts.map(_.label).mkString_(", ")}"
          ).leftIor.toIorNec
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
          s"Unknown enum value: $name. Available values: ${fromName.keys.mkString(", ")}",
          range.some,
        )
      )
      .toIorNec
  }

  def suspend[A](f: => PartialCompiler[A]): PartialCompiler[A] = todo

  def bijection[A, B](
    f: PartialCompiler[A],
    to: A => B,
    from: B => A,
  ): PartialCompiler[B] = f.map(to)

  val timestamp: PartialCompiler[Timestamp] = stringLiteral.emap { s =>
    Timestamp
      // todo unhardcode format
      // todo: also, this keeps throwing in an uncatchable way
      .parse(s.value, TimestampFormat.DATE_TIME)
      .toRightIor(CompilationError("Invalid timestamp format", Some(s.range)))
      .toIorNec
  }

  def withHints[A](fa: PartialCompiler[A], hints: Hints): PartialCompiler[A] = fa // todo

  def document: PartialCompiler[Document] = todo

}
