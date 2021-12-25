package playground

import cats.Id
import cats.data.Const
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Semigroup
import cats.parse.Parser

trait Tokens[F[_]] {
  def comment(text: String): F[Unit]

  def liftToken[A](text: String, value: A): F[A]
  def liftParser(parser: Parser[String]): Parser[F[String]] = parser.map(s => liftToken(s, s))
  def block[A](fa: F[A]): F[A]
}

object Tokens {

  type Lexer[A] = Const[TokenTree, A]

  val idTokens: Tokens[Id] =
    new Tokens[Id] {
      def comment(text: String): Unit = ()
      def liftToken[A](text: String, value: A): A = value
      def block[A](fa: A): A = fa
    }

  val lexerTokens: Tokens[Lexer] =
    new Tokens[Lexer] {

      def comment(text: String): Lexer[Unit] = Const(
        TokenTree.Single(Token.Simple("//" + text))
      )

      def liftToken[A](text: String, value: A): Lexer[A] = Const(
        TokenTree.Single(Token.Simple(text))
      )

      def block[A](fa: Lexer[A]): Lexer[A] = Const {
        TokenTree.Nested(NonEmptyList.one(fa.getConst))
      }

    }

}

case class Token(value: String)

object Token {
  def Simple(s: String): Token = Token(s)
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
