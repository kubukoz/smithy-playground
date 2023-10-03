package playground.smithyql.parser.v2

import cats.Show
import cats.effect.IO
import cats.implicits._
import cats.parse.Numbers
import com.softwaremill.diffx.Diff
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import playground.Assertions
import playground.smithyql.parser.v2.scanner.Scanner
import playground.smithyql.parser.v2.scanner.Token
import playground.smithyql.parser.v2.scanner.TokenKind
import playground.smithyql.parser.v2.scanner.TokenKind._
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

  scanTest("{")(List(LBR("{")))
  scanTest("}")(List(RBR("}")))
  scanTest("[")(List(LB("[")))
  scanTest("]")(List(RB("]")))
  scanTest(".")(List(DOT(".")))
  scanTest(",")(List(COMMA(",")))
  scanTest("#")(List(HASH("#")))
  scanTest(":")(List(COLON(":")))
  scanTest("=")(List(EQ("=")))
  scanTest("a")(List(IDENT("a")))
  scanTest("use")(List(KW_USE("use")))
  scanTest("service")(List(KW_SERVICE("service")))
  scanTest("null")(List(KW_NULL("null")))
  scanTest("true")(List(KW_BOOLEAN("true")))
  scanTest("false")(List(KW_BOOLEAN("false")))

  scanTest("5")(List(LIT_NUMBER("5")))
  scanTest("50")(List(LIT_NUMBER("50")))

  // todo: this would be nice to parse as a single error token.
  // might be possible to achieve by catching epsilon failures in the number parser, so that if any progress is seen we'd skip N characters before another token is attempted.
  // need to test this for interactions with other following tokens (as well as error tokens before numbers, which are using readOne).
  scanTest("05")(List(LIT_NUMBER("0"), LIT_NUMBER("5")))
  scanTest("0")(List(LIT_NUMBER("0")))
  scanTest("0.0")(List(LIT_NUMBER("0.0")))
  scanTest("0.5")(List(LIT_NUMBER("0.5")))
  // tbh: this might work better as a single error token.
  // see above comment about epsilon failures.
  scanTest("0.")(List(Error("0"), DOT(".")))

  scanTest("1e10")(List(LIT_NUMBER("1e10")))

  private def numberTest[A: Arbitrary: Show](
    name: String
  ) =
    test(s"Any $name can be parsed as a number") {
      forall { (a: A) =>
        Assertions.assertNoDiff(scan(a.toString()), List(LIT_NUMBER(a.toString())))
      }
    }

  numberTest[Byte]("byte")
  numberTest[Short]("short")
  numberTest[Int]("int")
  numberTest[Long]("long")
  numberTest[Float]("float")
  numberTest[Double]("double")
  numberTest[BigInt]("bigint")
  // deliberately not testing BigDecimal this way - these are wider than json numbers so we can't test the full range

  test("If cats-parse can parse a JSON number, so can we") {
    forall { (s: String) =>
      Numbers.jsonNumber.parseAll(s).toOption match {
        case None => success
        case Some(succ) =>
          println("woop woop!")
          Assertions.assertNoDiff(scan(succ), List(LIT_NUMBER(succ)))
      }
    }
  }

  // idents
  scanTest("abcdef")(List(IDENT("abcdef")))

  scanTest(
    "hello_world"
  )(
    List(
      IDENT("hello_world")
    )
  )

  scanTest(
    "helloworld123"
  )(
    List(
      IDENT("helloworld123")
    )
  )

  scanTest(explicitName = "Identifier similar to a keyword - prefix", input = "notfalse")(
    List(
      IDENT("notfalse")
    )
  )

  scanTest(explicitName = "Identifier similar to a keyword - suffix", input = "falsely")(
    List(
      IDENT("falsely")
    )
  )

  // whitespace
  scanTest(" ")(List(SPACE(" ")))
  scanTest("\n")(List(NEWLINE("\n")))

  // contiguous whitespace of all kinds
  // notably newlines are grouped together separately from other whitespace
  scanTest("  \r \r \n\n")(List(SPACE("  \r \r "), NEWLINE("\n\n")))
  scanTest("  \n\n  \n ")(
    List(
      SPACE("  "),
      NEWLINE("\n\n"),
      SPACE("  "),
      NEWLINE("\n"),
      SPACE(" "),
    )
  )

  // comments
  scanTest("// hello 123 foo bar --")(List(COMMENT("// hello 123 foo bar --")))

  scanTest(
    explicitName = "Scan multiple line-comments",
    input =
      """//hello
        |//world""".stripMargin,
  )(
    List(
      COMMENT("//hello"),
      NEWLINE("\n"),
      COMMENT("//world"),
    )
  )

  scanTest(
    "hello world //this is a comment"
  )(
    List(
      IDENT("hello"),
      SPACE(" "),
      IDENT("world"),
      SPACE(" "),
      COMMENT("//this is a comment"),
    )
  )

  // errors

  scanTest(
    explicitName = "Error tokens for input that doesn't match any other token",
    input = "🤷*%$^@-+?",
  )(List(Error("🤷*%$^@-+?")))

  scanTest(
    explicitName = "Error tokens mixed between other tokens",
    input = "hello@world",
  )(
    List(
      IDENT("hello"),
      Error("@"),
      IDENT("world"),
    )
  )

  scanTest(
    explicitName = "Error tokens mixed between other tokens - complex",
    input = "hello@world-this?is=an<example",
  )(
    List(
      IDENT("hello"),
      Error("@"),
      IDENT("world"),
      Error("-"),
      IDENT("this"),
      Error("?"),
      IDENT("is"),
      EQ("="),
      IDENT("an"),
      Error("<"),
      IDENT("example"),
    )
  )

  scanTest(explicitName = "Error tokens before a multi-char keyword", input = "--false")(
    List(
      Error("--"),
      KW_BOOLEAN("false"),
    )
  )

  // complex cases

  scanTestReverse(
    "many tokens of punctuation and idents mixed with error nodes and comments"
  )(
    List(
      LBR("{"),
      IDENT("foo"),
      RBR("}"),
      LB("["),
      IDENT("bar"),
      RB("]"),
      DOT("."),
      IDENT("baz"),
      COMMA(","),
      IDENT("xx"),
      NEWLINE("\n"),
      HASH("#"),
      COLON(":"),
      EQ("="),
      IDENT("abc123def"),
      SPACE(" "),
      IDENT("ghe"),
      Error("--"),
      IDENT("eef"),
      SPACE(" "),
      NEWLINE("\n"),
      COMMENT("//hello"),
      NEWLINE("\n"),
    )
  )

  scanTest(
    explicitName = "whitespace and comments around keyword",
    input =
      """hello use service foo //bar
        | true //one
        |//two
        |null """.stripMargin,
  )(
    List(
      IDENT("hello"),
      SPACE(" "),
      KW_USE("use"),
      SPACE(" "),
      KW_SERVICE("service"),
      SPACE(" "),
      IDENT("foo"),
      SPACE(" "),
      COMMENT("//bar"),
      NEWLINE("\n"),
      SPACE(" "),
      KW_BOOLEAN("true"),
      SPACE(" "),
      COMMENT("//one"),
      NEWLINE("\n"),
      COMMENT("//two"),
      NEWLINE("\n"),
      KW_NULL("null"),
      SPACE(" "),
    )
  )

  // string literals
  scanTest(
    "\"hello world\""
  )(
    List(
      LIT_STRING("\"hello world\"")
    )
  )

  scanTest(
    explicitName = "String literal that never ends",
    input = "\"hello world",
  )(
    List(
      LIT_STRING("\"hello world")
    )
  )

  scanTest(
    explicitName = "Multiple string literals",
    input = "\"hello world\", \"foo bar\"",
  )(
    List(
      LIT_STRING("\"hello world\""),
      COMMA(","),
      SPACE(" "),
      LIT_STRING("\"foo bar\""),
    )
  )

  scanTest(
    explicitName = "Multiple string literals, second one not closed",
    input = "\"hello world\", \"foo bar",
  )(
    List(
      LIT_STRING("\"hello world\""),
      COMMA(","),
      SPACE(" "),
      LIT_STRING("\"foo bar"),
    )
  )

  scanTest(
    explicitName = "Multiple string literals, first one not closed",
    input = "\"hello world, \"foo bar\"",
  )(
    List(
      LIT_STRING("\"hello world, \""),
      IDENT("foo"),
      SPACE(" "),
      IDENT("bar"),
      LIT_STRING("\""),
    )
  )

  scanTest(
    explicitName = "String literal, multi-line (parity test)",
    input = "\"hello\nworld\"",
  )(
    List(
      LIT_STRING("\"hello\nworld\"")
    )
  )

  // real files

  scanTest(
    explicitName = "Real file 1",
    input =
      """use service demo.smithy#DemoService
        |
        |// CreateSubscription {
        |//   subscription: {
        |//     id: "subscription_id",
        |//     name: "name",
        |//     createdAt: "2020-04-01T00:00:00Z",
        |//   },
        |// }
        |CreateHero {
        |  hero: {
        |    good: // bgasdfasldf
        |      {
        |        howGood: 10,
        |      },
        |  },
        |  intSet: [
        |    1,
        |    2,
        |    1,
        |  ],
        |}
        |""".stripMargin,
  )(
    List(
      KW_USE("use"),
      SPACE(" "),
      KW_SERVICE("service"),
      SPACE(" "),
      IDENT("demo"),
      DOT("."),
      IDENT("smithy"),
      HASH("#"),
      IDENT("DemoService"),
      NEWLINE("\n\n"),
      COMMENT("// CreateSubscription {"),
      NEWLINE("\n"),
      COMMENT("//   subscription: {"),
      NEWLINE("\n"),
      COMMENT("//     id: \"subscription_id\","),
      NEWLINE("\n"),
      COMMENT("//     name: \"name\","),
      NEWLINE("\n"),
      COMMENT("//     createdAt: \"2020-04-01T00:00:00Z\","),
      NEWLINE("\n"),
      COMMENT("//   },"),
      NEWLINE("\n"),
      COMMENT("// }"),
      NEWLINE("\n"),
      IDENT("CreateHero"),
      SPACE(" "),
      LBR("{"),
      NEWLINE("\n"),
      SPACE("  "),
      IDENT("hero"),
      COLON(":"),
      SPACE(" "),
      LBR("{"),
      NEWLINE("\n"),
      SPACE("    "),
      IDENT("good"),
      COLON(":"),
      SPACE(" "),
      COMMENT("// bgasdfasldf"),
      NEWLINE("\n"),
      SPACE("      "),
      LBR("{"),
      NEWLINE("\n"),
      SPACE("        "),
      IDENT("howGood"),
      COLON(":"),
      SPACE(" "),
      LIT_NUMBER("10"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("      "),
      RBR("}"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("  "),
      RBR("}"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("  "),
      IDENT("intSet"),
      COLON(":"),
      SPACE(" "),
      LB("["),
      NEWLINE("\n"),
      SPACE("    "),
      LIT_NUMBER("1"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("    "),
      LIT_NUMBER("2"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("    "),
      LIT_NUMBER("1"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("  "),
      RB("]"),
      COMMA(","),
      NEWLINE("\n"),
      RBR("}"),
      NEWLINE("\n"),
    )
  )
}
