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

object QueryCompilerSchematic {
  type WAST = AST[WithSource]
}

trait PartialCompiler[A] {
  def emap[B](f: A => IorNec[CompilationError, B]): PartialCompiler[B] =
    ast => compile(ast).flatMap(f)

  def compile(ast: AST[WithSource]): IorNec[CompilationError, A]
}

object PartialCompiler {

  implicit val functor: Functor[PartialCompiler] = Derive.functor

  type WAST = AST[WithSource]

  val unit: PartialCompiler[Unit] = _ => ().rightIor

  def fromPF[A](
    f: PartialFunction[WAST, A]
  )(
    orElseMessage: WAST => String
  ): PartialCompiler[A] =
    ast => f.lift(ast).toRightIor(NonEmptyChain(CompilationError(orElseMessage(ast))))

}

//todo adt
final case class CompilationError(message: String)

class QueryCompilerSchematic
  extends smithy4s.Schematic[PartialCompiler]
  with schematic.struct.GenericAritySchematic[PartialCompiler] {
  def short: PartialCompiler[Short] = ???

  def int: PartialCompiler[Int] =
    PartialCompiler.fromPF { case IntLiteral(i) => i.value }(ast =>
      s"Expected ${NodeKind.IntLiteral}, got ${ast.kind} instead"
    )

  def long: PartialCompiler[Long] = ???

  def double: PartialCompiler[Double] = ???

  def float: PartialCompiler[Float] = ???

  def bigint: PartialCompiler[BigInt] = ???

  def bigdecimal: PartialCompiler[BigDecimal] = ???

  def string: PartialCompiler[String] =
    PartialCompiler.fromPF { case StringLiteral(s) => s.value }(ast =>
      s"Expected ${NodeKind.StringLiteral}, got ${ast.kind} instead"
    )

  def boolean: PartialCompiler[Boolean] = ???

  def uuid: PartialCompiler[UUID] = ???

  def byte: PartialCompiler[Byte] = ???

  def bytes: PartialCompiler[ByteArray] = ???

  def unit: PartialCompiler[Unit] = PartialCompiler.unit

  def list[S](fs: PartialCompiler[S]): PartialCompiler[List[S]] = ???

  def set[S](fs: PartialCompiler[S]): PartialCompiler[Set[S]] = ???

  def vector[S](fs: PartialCompiler[S]): PartialCompiler[Vector[S]] = ???

  def map[K, V](fk: PartialCompiler[K], fv: PartialCompiler[V]): PartialCompiler[Map[K, V]] = ???

  def genericStruct[S](
    fields: Vector[Field[PartialCompiler, S, _]]
  )(
    const: Vector[Any] => S
  ): PartialCompiler[S] = PartialCompiler
    .fromPF { case Struct(asts) => asts }(ast =>
      s"Expected ${NodeKind.Struct}, got ${ast.kind} instead"
    )
    .emap { struct =>
      fields
        .traverse { field =>
          val fieldOpt = struct
            .value
            .value
            .find(_._1.value.text == field.label)
            .map(_._2)
            .traverse(field.instance.compile)

          if (field.isOptional)
            fieldOpt
          else
            fieldOpt.flatMap {
              _.toRightIor(CompilationError(s"Missing field ${field.label}")).toIorNec
            }
        }
    }
    .map(const)

  def union[S](
    first: Alt[PartialCompiler, S, _],
    rest: Vector[Alt[PartialCompiler, S, _]],
  )(
    total: S => Alt.WithValue[PartialCompiler, S, _]
  ): PartialCompiler[S] = {
    val opts = NonEmptyList(first, rest.toList)

    PartialCompiler
      .fromPF { case s @ Struct(_) => s }(ast =>
        s"Expected a union struct, got ${ast.kind} instead"
      )
      .emap {
        case s if s.fields.value.value.size == 1 =>
          val defs = s.fields.value.value
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

        case s if s.fields.value.value.isEmpty =>
          CompilationError(
            "found empty struct, expected one of: " + opts.map(_.label).mkString_(", ")
          ).leftIor.toIorNec

        case Struct(defs) =>
          CompilationError(
            s"struct mismatch (keys: ${defs.value.value.keys.map(_.value.text).toList.mkString_(", ")}), you must choose exactly one of: ${opts.map(_.label).mkString_(", ")}"
          ).leftIor.toIorNec
      }
  }

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): PartialCompiler[A] = ???

  def suspend[A](f: => PartialCompiler[A]): PartialCompiler[A] = ???

  def bijection[A, B](
    f: PartialCompiler[A],
    to: A => B,
    from: B => A,
  ): PartialCompiler[B] = f.map(to)

  def timestamp: PartialCompiler[Timestamp] = string.emap { s =>
    Timestamp
      /*  todo unhardcode format */
      .parse(s, TimestampFormat.DATE_TIME)
      .toRightIor(CompilationError("Invalid timestamp format"))
      .toIorNec
  }

  def withHints[A](fa: PartialCompiler[A], hints: Hints): PartialCompiler[A] = fa // todo

  def document: PartialCompiler[Document] = ???

}
