package playground.smithyql.parser.v2

import cats.effect.IO
import cats.implicits._
import com.softwaremill.diffx.Diff
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import playground.Assertions
import playground.smithyql.parser.v2.scanner.Scanner
import playground.smithyql.parser.v2.scanner.Token
import playground.smithyql.parser.v2.scanner.TokenKind
import weaver._
import weaver.scalacheck.Checkers

import Scanner.scan

object ScannerTests extends SimpleIOSuite with Checkers {

  implicit val tokenKindDiff: Diff[TokenKind] = Diff.derived
  implicit val tokenDiff: Diff[Token] = Diff.derived

  def arbTests(
    name: TestName
  )(
    withArb: Arbitrary[String] => IO[Expectations]
  ): Unit = {

    val sampleStringGen = Gen.oneOf(
      Gen.alphaStr,
      Gen.alphaNumStr,
      Gen.asciiPrintableStr,
      Gen.identifier,
      Gen.oneOf(List(' ', '\n', '\t', '\r', '\f', '\b')).map(_.toString),
    )

    val arbString: Arbitrary[String] = Arbitrary {
      Gen.listOf(sampleStringGen).map(_.mkString)
    }

    test(name)(withArb(Arbitrary.arbString))
    test(name.copy(name = name.name + " (prepared input)"))(withArb(arbString))
  }

  arbTests("Any string input scans successfully") { implicit arbString =>
    forall { (s: String) =>
      scan(s): Unit
      success
    }
  }

  arbTests("Scanning is lossless") { implicit arbString =>
    forall { (s: String) =>
      assert.eql(scan(s).foldMap(_.text), s)
    }
  }

  private def scanTest(
    input: String,
    explicitName: TestName = "",
  )(
    expected: List[Token]
  )(
    implicit loc: SourceLocation
  ): Unit =
    pureTest(
      if (explicitName.name.nonEmpty)
        explicitName
      else
        "Scan string: " + sanitize(input)
    ) {
      Assertions.assertNoDiff(scan(input), expected)
    }

  // Runs scanTest by first rendering the expected tokens to a string, then scanning it to get them back.
  // If the output is not the same as the input, the test fails.
  // While it's guaranteed that rendering tokens to text produces scannable code (everything is scannable),
  // due to ambiguities in the scanner it's not guaranteed that the output will be the same as the input - hence the need to test.
  private def scanTestReverse(
    explicitName: String
  )(
    expected: List[Token]
  )(
    implicit loc: SourceLocation
  ): Unit = scanTest(expected.foldMap(_.text), explicitName)(expected)

  private def sanitize(
    text: String
  ) = text.replace(" ", "·").replace("\n", "↵")

  scanTest("{")(List(TokenKind.LBR("{")))
  scanTest("}")(List(TokenKind.RBR("}")))
  scanTest("[")(List(TokenKind.LB("[")))
  scanTest("]")(List(TokenKind.RB("]")))
  scanTest(".")(List(TokenKind.DOT(".")))
  scanTest(",")(List(TokenKind.COMMA(",")))
  scanTest("#")(List(TokenKind.HASH("#")))
  scanTest(":")(List(TokenKind.COLON(":")))
  scanTest("=")(List(TokenKind.EQ("=")))
  scanTest("a")(List(TokenKind.IDENT("a")))

  // idents
  scanTest("abcdef")(List(TokenKind.IDENT("abcdef")))

  scanTest(
    "hello_world"
  )(
    List(
      TokenKind.IDENT("hello_world")
    )
  )

  scanTest(
    "helloworld123"
  )(
    List(
      TokenKind.IDENT("helloworld123")
    )
  )

  // whitespace
  scanTest(" ")(List(TokenKind.SPACE(" ")))
  scanTest("\n")(List(TokenKind.NEWLINE("\n")))

  // contiguous whitespace of all kinds
  // notably newlines are grouped together separately from other whitespace
  scanTest("  \r \r \n\n")(List(TokenKind.SPACE("  \r \r "), TokenKind.NEWLINE("\n\n")))
  scanTest("  \n\n  \n ")(
    List(
      TokenKind.SPACE("  "),
      TokenKind.NEWLINE("\n\n"),
      TokenKind.SPACE("  "),
      TokenKind.NEWLINE("\n"),
      TokenKind.SPACE(" "),
    )
  )

  // comments
  scanTest("// hello 123 foo bar --")(List(TokenKind.COMMENT("// hello 123 foo bar --")))

  scanTest(
    explicitName = "Scan multiple line-comments",
    input =
      """//hello
        |//world""".stripMargin,
  )(
    List(
      TokenKind.COMMENT("//hello"),
      TokenKind.NEWLINE("\n"),
      TokenKind.COMMENT("//world"),
    )
  )

  scanTest(
    "hello world //this is a comment"
  )(
    List(
      TokenKind.IDENT("hello"),
      TokenKind.SPACE(" "),
      TokenKind.IDENT("world"),
      TokenKind.SPACE(" "),
      TokenKind.COMMENT("//this is a comment"),
    )
  )

  // errors

  scanTest(
    explicitName = "Error tokens for input that doesn't match any other token",
    input = "🤷*%$^@-+?",
  )(List(TokenKind.Error("🤷*%$^@-+?")))

  scanTest(
    explicitName = "Error tokens mixed between other tokens",
    input = "hello@world",
  )(
    List(
      TokenKind.IDENT("hello"),
      TokenKind.Error("@"),
      TokenKind.IDENT("world"),
    )
  )

  scanTest(
    explicitName = "Error tokens mixed between other tokens - complex",
    input = "hello@world-this?is=an<example",
  )(
    List(
      TokenKind.IDENT("hello"),
      TokenKind.Error("@"),
      TokenKind.IDENT("world"),
      TokenKind.Error("-"),
      TokenKind.IDENT("this"),
      TokenKind.Error("?"),
      TokenKind.IDENT("is"),
      TokenKind.EQ("="),
      TokenKind.IDENT("an"),
      TokenKind.Error("<"),
      TokenKind.IDENT("example"),
    )
  )

  // complex cases

  scanTestReverse(
    "many tokens of punctuation and idents mixed with error nodes and comments"
  )(
    List(
      TokenKind.LBR("{"),
      TokenKind.IDENT("foo"),
      TokenKind.RBR("}"),
      TokenKind.LB("["),
      TokenKind.IDENT("bar"),
      TokenKind.RB("]"),
      TokenKind.DOT("."),
      TokenKind.IDENT("baz"),
      TokenKind.COMMA(","),
      TokenKind.IDENT("xx"),
      TokenKind.NEWLINE("\n"),
      TokenKind.HASH("#"),
      TokenKind.COLON(":"),
      TokenKind.EQ("="),
      TokenKind.IDENT("abc123def"),
      TokenKind.SPACE(" "),
      TokenKind.IDENT("ghe"),
      TokenKind.Error("--"),
      TokenKind.IDENT("eef"),
      TokenKind.SPACE(" "),
      TokenKind.NEWLINE("\n"),
      TokenKind.COMMENT("//hello"),
      TokenKind.NEWLINE("\n"),
    )
  )
}
