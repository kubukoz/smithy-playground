package playground

import cats.Apply
import cats.data.IorNec
import cats.data.Kleisli
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
      ): QueryCompiler[B] = Kleisli(fa.compile).map(f).run(_)

      def ap[A, B](
        ff: QueryCompiler[A => B]
      )(
        fa: QueryCompiler[A]
      ): QueryCompiler[B] = Kleisli(ff.compile).parAp(Kleisli(fa.compile)).run(_)

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
