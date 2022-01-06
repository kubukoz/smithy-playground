package playground.smithyql

import cats.Apply
import cats.Eval
import cats.Id
import cats.NonEmptyTraverse
import cats.implicits._
import cats.~>

// todo: multiline
final case class Comment(text: String)

final case class WithSource[+A](
  commentsLeft: List[Comment],
  commentsRight: List[Comment],
  value: A,
) {
  def allComments(aTokens: A => List[Comment]): List[Comment] =
    commentsLeft ++ aTokens(value) ++ commentsRight

  def tokensEach: List[Comment] = commentsLeft ++ commentsRight

  def between(
    lhs: List[Comment],
    rhs: List[Comment],
  ) = WithSource(
    commentsLeft = lhs ++ commentsLeft,
    value = value,
    commentsRight = commentsRight ++ rhs,
  )

}

object WithSource {

  def allQueryComments(q: Query[WithSource]): List[Comment] = {

    def comments(node: InputNode[WithSource]): List[Comment] = node.fold(
      struct = _.fields.allComments(_.allComments(_.flatMap { case (k, v) =>
        k.allComments(_ => Nil) ++ v.fold(comments, comments, comments)
      }.toList)),
      string = _.value.allComments(_ => Nil),
      int = _.value.allComments(_ => Nil),
    )

    q.operationName.allComments(_ => Nil) ++
      q.input
        .fold(
          comments,
          comments,
          comments,
        )
  }

  implicit val instances: NonEmptyTraverse[WithSource] =
    new NonEmptyTraverse[WithSource] {

      def foldLeft[A, B](fa: WithSource[A], b: B)(f: (B, A) => B): B = f(b, fa.value)

      def foldRight[A, B](
        fa: WithSource[A],
        lb: Eval[B],
      )(
        f: (A, Eval[B]) => Eval[B]
      ): Eval[B] = f(fa.value, lb)

      def reduceLeftTo[A, B](fa: WithSource[A])(f: A => B)(g: (B, A) => B): B = f(fa.value)

      def reduceRightTo[A, B](fa: WithSource[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        Eval.later(f(fa.value))

      def nonEmptyTraverse[G[_]: Apply, A, B](fa: WithSource[A])(f: A => G[B]): G[WithSource[B]] =
        f(fa.value).map(v => fa.copy(value = v))

    }

  val unwrap: WithSource ~> Id =
    new (WithSource ~> Id) {
      def apply[A](wa: WithSource[A]): A = wa.value
    }

}
