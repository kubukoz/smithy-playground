package playground.smithyql.parser.v2.scanner

import cats.kernel.Eq
import cats.parse.Numbers
import cats.syntax.all.*

import scala.annotation.nowarn

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
  case object LIT_NUMBER extends TokenKind
  case object LIT_STRING extends TokenKind
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
      token: String,
      tok: TokenKind,
    ): PartialFunction[Unit, Unit] = {
      case _ if remaining.startsWith(token) =>
        add(tok(token.toString))
        remaining = remaining.drop(token.length())
    }

    def simpleTokens(
      pairings: (
        String,
        TokenKind,
      )*
    ): PartialFunction[Unit, Unit] = pairings.map(readSimple.tupled).reduce(_.orElse(_))

    val keywords = Map(
      "use" -> TokenKind.KW_USE,
      "service" -> TokenKind.KW_SERVICE,
      "null" -> TokenKind.KW_NULL,
      "true" -> TokenKind.KW_BOOLEAN,
      "false" -> TokenKind.KW_BOOLEAN,
    )

    val readIdent: PartialFunction[Unit, Unit] = {
      case _ if remaining.head.isLetter =>
        val (letters, rest) = remaining.span(ch => ch.isLetterOrDigit || ch == '_')

        keywords.get(letters) match {
          case Some(kind) =>
            // we matched a keyword, return it.
            add(kind(letters))

          case None =>
            // normal ident
            add(TokenKind.IDENT(letters))
        }

        remaining = rest
    }

    val readPunctuation: PartialFunction[Unit, Unit] = simpleTokens(
      "." -> TokenKind.DOT,
      "," -> TokenKind.COMMA,
      "#" -> TokenKind.HASH,
      "[" -> TokenKind.LB,
      "]" -> TokenKind.RB,
      "{" -> TokenKind.LBR,
      "}" -> TokenKind.RBR,
      ":" -> TokenKind.COLON,
      "=" -> TokenKind.EQ,
    )

    val readStringLiteral: PartialFunction[Unit, Unit] = {
      case _ if remaining.startsWith("\"") =>
        val (str, rest) = remaining.tail.span(_ != '\"')
        if (rest.isEmpty) { // hit EOF
          add(TokenKind.LIT_STRING("\"" + str))
          remaining = rest
        } else {
          add(TokenKind.LIT_STRING("\"" + str + "\""))
          remaining = rest.tail
        }
    }

    val readNumberLiteral: PartialFunction[Unit, Unit] = {
      // I love this language
      object jsonNumber {
        def unapply(
          @nowarn("cat=unused")
          unused: Unit
        ): Option[
          (
            String,
            String,
          )
        ] =
          // For now, we're using the cats-parse implementation simply because it's consistent with the current implementation
          // and we can rewrite this later on when we drop support for the other parser
          // and no longer need cats-parse.
          Numbers.jsonNumber.parse(remaining).toOption
      }

      { case jsonNumber(rest, num) =>
        add(TokenKind.LIT_NUMBER(num.toString))
        remaining = rest
      }
    }

    // readOne and friends are all partial functions: this is the current implementation of lookahead.
    // it's not great, but it kinda works.
    val readOne: PartialFunction[Unit, Unit] = readIdent
      .orElse(readPunctuation)
      .orElse(readStringLiteral)
      .orElse(readNumberLiteral)

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

    val eatWhitespace: PartialFunction[Unit, Unit] = {
      object matches {
        def unapply(
          @nowarn("cat=unused") u: Unit
        ): Option[
          (
            String,
            String,
          )
        ] = {
          val (wsp, rest) = remaining.span(ch => ch.isWhitespace)
          if (wsp.isEmpty())
            None
          else
            Some((wsp, rest))
        }
      }

      { case matches(wsp, rest) =>
        whitespaceChains(wsp).foreach(add)
        remaining = rest
      }
    }

    val eatComments: PartialFunction[Unit, Unit] = {
      case _ if remaining.startsWith("//") =>
        while (remaining.startsWith("//")) {
          val (comment, rest) = remaining.span(_ != '\n')
          add(TokenKind.COMMENT(comment))
          remaining = rest
        }
    }

    val readAny = readOne.orElse(eatWhitespace).orElse(eatComments)

    def eatErrors(
    ) = {
      val (failures, _) = remaining.span { _ =>
        if (readAny.isDefinedAt(()))
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

      readAny
        .lift(())
        .isDefined ||
      eatErrors()

      // last-effort sanity check
      if (remaining == last)
        sys.error(s"no progress in the last run! remaining string: $remaining")
    }

    tokens.reverse
  }

}
