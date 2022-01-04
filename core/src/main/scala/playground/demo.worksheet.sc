import playground.Tokens

import playground.SmithyQLParser
val input = """
CreateHero { //bar
hero = { //foo
//    bad = {
//      evilName = "evil",
//      powerLevel = 9001,
//    },
    good = {
      howGood = //100
      //  200,
      200,
      anotherKey//foo = "a"
        = 42,
    },
  },
}
"""

SmithyQLParser.parser(Tokens.lexerTokens).parseAll(input).toOption.get.getConst.display(0)

val example = """
{
  k = v,
  k2 = v2,
}
"""

val exampleFull = """
// comment before
{
  //before key
  k //before equals
  = //before value
  v //before comma
  , //between keys
  k2 = v2,
  // after keys
}
// comment after
"""

import cats.parse._
import cats.parse.Parser._
val comment = string("//") *> charsWhile0(_ != '\n') <* char('\n')

val whitespace: Parser0[Unit] =
  comment
    .repSep0(charsWhile0(_.isWhitespace))
    .surroundedBy(charsWhile0(_.isWhitespace))
    .void

def token[A](parser: Parser[A]): Parser[A] = parser.surroundedBy(whitespace)
