package playground.smithyql.parser.v2

import cats.Show
import cats.implicits._
import cats.parse.Numbers
import org.scalacheck.Arbitrary
import playground.Assertions
import playground.smithyql.parser.v2.scanner.Scanner
import playground.smithyql.parser.v2.scanner.TokenKind._
import weaver._
import weaver.scalacheck.Checkers

import Diffs.given
import Scanner.scan

object ScannerTests extends SimpleIOSuite with Checkers with ScannerSuite {

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

  scanTest(explicitName = "Error tokens before a comment", input = "--//hello")(
    List(
      Error("--"),
      COMMENT("//hello"),
    )
  )

  scanTest(explicitName = "Error tokens before whitespace", input = "--  ")(
    List(
      Error("--"),
      SPACE("  "),
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
}
