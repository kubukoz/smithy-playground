package playground

import cats.Apply
import cats.data.IorNec
import cats.data.NonEmptyChain
import cats.syntax.all.*
import playground.CompilationErrorDetails.*
import playground.smithyql.*

import QueryCompiler.WAST

trait QueryCompiler[A] {

  final def emap[B](
    f: A => QueryCompiler.Result[B]
  ): QueryCompiler[B] = ast => compile(ast).flatMap(f)

  def compile(
    ast: WAST
  ): QueryCompiler.Result[A]

}

object QueryCompiler {
  type Result[+A] = IorNec[CompilationError, A]

  implicit val apply: Apply[QueryCompiler] =
    new Apply[QueryCompiler] {

      def map[A, B](
        fa: QueryCompiler[A]
      )(
        f: A => B
      ): QueryCompiler[B] = fa.compile(_).map(f)

      def ap[A, B](
        ff: QueryCompiler[A => B]
      )(
        fa: QueryCompiler[A]
      ): QueryCompiler[B] =
        wast =>
          (ff.compile(wast), fa.compile(wast)).parMapN(
            (
              a,
              b,
            ) => a(b)
          )

    }

  type WAST = WithSource[InputNode[WithSource]]

  val pos: QueryCompiler[SourceRange] = _.range.rightIor
  val unit: QueryCompiler[Unit] = _ => ().rightIor

  def typeCheck[A](
    expected: NodeKind
  )(
    f: PartialFunction[InputNode[WithSource], A]
  ): QueryCompiler[WithSource[A]] =
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
