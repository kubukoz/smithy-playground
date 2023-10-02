package playground.smithyql.parser.v2.scanner

import cats.kernel.Eq
import cats.syntax.all.*

case class Token(
  kind: TokenKind,
  text: String,
) {
  def width: Int = text.length
}

object Token {
  implicit val eq: Eq[Token] = Eq.fromUniversalEquals
}

sealed trait TokenKind extends Product with Serializable {

  def apply(
    text: String
  ): Token = Token(this, text)

}

object TokenKind {
  case object KW_IMPORT extends TokenKind
  case object DOT extends TokenKind
  case object COMMA extends TokenKind
  case object HASH extends TokenKind
  case object LB extends TokenKind
  case object RB extends TokenKind
  case object LBR extends TokenKind
  case object RBR extends TokenKind
  case object COLON extends TokenKind
  case object EQ extends TokenKind
  case object SPACE extends TokenKind
  case object NEWLINE extends TokenKind
  case object IDENT extends TokenKind
  case object COMMENT extends TokenKind
  case object Error extends TokenKind

  implicit val eq: Eq[TokenKind] = Eq.fromUniversalEquals
}

object Scanner {

  /** Entrypoint to scanning text into tokens.
    *
    * Always produces an output that can be rendered back to the original text.
    */
  def scan(
    s: String
  ): List[Token] = {
    var remaining = s
    var tokens = List.empty[Token]
    def add(
      tok: Token
    ) = tokens ::= tok

    def readSimple(
      token: Char,
      tok: TokenKind,
    ): PartialFunction[Char, Unit] = { case `token` =>
      add(tok(token.toString))
      remaining = remaining.tail
    }

    def simpleTokens(
      pairings: (
        Char,
        TokenKind,
      )*
    ): PartialFunction[Char, Unit] = pairings
      .map(readSimple.tupled)
      .reduce(_ orElse _)

    val readOne: PartialFunction[Char, Unit] = simpleTokens(
      '.' -> TokenKind.DOT,
      ',' -> TokenKind.COMMA,
      '#' -> TokenKind.HASH,
      '[' -> TokenKind.LB,
      ']' -> TokenKind.RB,
      '{' -> TokenKind.LBR,
      '}' -> TokenKind.RBR,
      ':' -> TokenKind.COLON,
      '=' -> TokenKind.EQ,
    ).orElse {
      case letter if letter.isLetter =>
        val (letters, rest) = remaining.span(ch => ch.isLetterOrDigit || ch == '_')
        add(TokenKind.IDENT(letters))
        remaining = rest
    }

    // split "whitespace" string into chains of contiguous newlines OR whitespace characters.
    def whitespaceChains(
      whitespace: String
    ): List[Token] = {
      val isNewline = (ch: Char) => ch == '\n'

      if (whitespace.isEmpty)
        Nil
      else if (isNewline(whitespace.head)) {
        val (nl, rest) = whitespace.span(isNewline)
        TokenKind.NEWLINE(nl) :: whitespaceChains(rest)
      } else {
        val (wsp, rest) = whitespace.span(!isNewline(_))
        TokenKind.SPACE(wsp) :: whitespaceChains(rest)
      }
    }

    def eatWhitespace(
    ) = {
      val (wsp, rest) = remaining.span(ch => ch.isWhitespace)
      if (wsp.isEmpty())
        false
      else {
        whitespaceChains(wsp).foreach(add)
        remaining = rest

        true
      }
    }

    def eatComments(
    ) =
      if (!remaining.startsWith("//"))
        false
      else {
        while (remaining.startsWith("//")) {
          val (comment, rest) = remaining.span(_ != '\n')
          add(TokenKind.COMMENT(comment))
          remaining = rest
        }

        true
      }

    def eatErrors(
    ) = {
      // todo: bug: even if the next character starts a multi-char token, this will consider it an error.
      // instead, we should rework "readOne" to consume arbitrary constant-length tokens, and also include the possibility that `rest` has comments or whitespace.
      val (failures, rest) = remaining.span(!readOne.isDefinedAt(_))
      remaining = rest
      if (failures.nonEmpty) {
        add(TokenKind.Error(failures))
        true
      } else
        false
    }

    while (remaining.nonEmpty) {
      val last = remaining

      readOne.applyOrElse[Char, Any](
        remaining.head,
        (_: Char) =>
          // nothing matched. Eat whitespace and see if the rest is an error
          eatWhitespace() || eatComments() || eatErrors(),
      )

      if (remaining == last)
        sys.error(s"no progress in the last run! remaining string: $remaining")
    }

    tokens.reverse
  }

}
