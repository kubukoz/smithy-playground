package playground

import cats.Id
import cats.data.Const
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Semigroup
import cats.parse.Parser
import playground.AST.Token
import playground.AST.WithSource

trait Tokens[F[_]] {
  def comment(text: String): F[Unit]

  def liftToken[A](token: => Token, value: A): F[A]

  def liftParser(
    parser: Parser[String]
  )(
    wrap: String => Token
  ): Parser[F[String]] = parser.map(s => liftToken(wrap(s), s))

  def block[A](fa: F[A]): F[A]
}

object Tokens {

  type Lexer[A] = Const[TokenTree, A]

  val idTokens: Tokens[Id] =
    new Tokens[Id] {
      def comment(text: String): Unit = ()
      def liftToken[A](token: => Token, value: A): A = value
      def block[A](fa: A): A = fa
    }

  val lexerTokens: Tokens[Lexer] =
    new Tokens[Lexer] {

      def comment(text: String): Lexer[Unit] = Const(
        TokenTree.Single(Token.Comment(text))
      )

      def liftToken[A](token: => Token, value: A): Lexer[A] = Const(
        TokenTree.Single(token)
      )

      def block[A](fa: Lexer[A]): Lexer[A] = Const {
        TokenTree.Nested(NonEmptyList.one(fa.getConst))
      }

    }

  val withSourceTokens: Tokens[WithSource] =
    new Tokens[WithSource] {
      def comment(text: String): WithSource[Unit] = WithSource((), List(Token.Comment(text)))

      def liftToken[A](token: => Token, value: A): WithSource[A] = WithSource(value, List(token))

      def block[A](fa: WithSource[A]): WithSource[A] = fa
    }

}

sealed trait TokenTree {

  def display(depth: Int): String =
    this match {
      case TokenTree.Single(content) => " " * 2 * depth + content
      case TokenTree.Nested(items) =>
        items
          .map(_.display(depth + 1))
          .mkString_(" " * 2 * depth + "(\n", "\n", s"\n${" " * 2 * depth})")
    }

}

object TokenTree {
  final case class Single(content: Token) extends TokenTree
  final case class Nested(content: NonEmptyList[TokenTree]) extends TokenTree

  implicit val sem: Semigroup[TokenTree] =
    new Semigroup[TokenTree] {

      def combine(x: TokenTree, y: TokenTree): TokenTree =
        (x, y) match {
          case (Nested(x), Nested(y)) => Nested(x.concatNel(y))
          case (Nested(x), r)         => Nested(x.append(r))
          case (l, Nested(y))         => Nested(y.prepend(l))
          case (l, r)                 => Nested(NonEmptyList.of(l, r))
        }

    }

}
