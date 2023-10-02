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
  case object KW_USE extends TokenKind
  case object KW_SERVICE extends TokenKind
  case object KW_BOOLEAN extends TokenKind
  case object KW_NUMBER extends TokenKind
  case object KW_STRING extends TokenKind
  case object KW_NULL extends TokenKind

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
    ): PartialFunction[Unit, Unit] = {
      case _ if remaining.startsWith(token.toString()) =>
        add(tok(token.toString))
        remaining = remaining.drop(token.toString().length())
    }

    def simpleTokens(
      pairings: (
        Char,
        TokenKind,
      )*
    ): PartialFunction[Unit, Unit] = pairings.map(readSimple.tupled).reduce(_.orElse(_))

    def readOne: PartialFunction[Unit, Unit] = simpleTokens(
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
      case _ if remaining.head.isLetter =>
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
      val (failures, _) = remaining.span { _ =>
        if (readOne.isDefinedAt(()))
          // this will match. stop!
          false
        else {
          // didn't match. We need to move the cursor manually here
          remaining = remaining.tail
          true
        }
      }

      if (failures.nonEmpty) {
        add(TokenKind.Error(failures))
        true
      } else
        false
    }

    while (remaining.nonEmpty) {
      val last = remaining

      {
        val matched = readOne.isDefinedAt(())
        if (matched)
          readOne(())

        matched
      } ||
        eatWhitespace() ||
        eatComments() ||
        eatErrors(): Unit

      // last-effort sanity check
      if (remaining == last)
        sys.error(s"no progress in the last run! remaining string: $remaining")
    }

    tokens.reverse
  }

}
