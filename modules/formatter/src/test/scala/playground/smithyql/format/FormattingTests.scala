package playground.smithyql.format

import playground.Assertions._
import playground.smithyql._
import playground.smithyql.format.Formatter
import playground.smithyql.parser.Examples
import playground.smithyql.parser.SourceParser
import weaver._
import weaver.scalacheck.Checkers
import util.chaining._
import DSL._
import Diffs._
import playground.smithyql.parser.ParserSuite

object FormattingTests extends SimpleIOSuite with Checkers {

  def formattingTest[Alg[_[_]]: Formatter: SourceParser](
    label: TestName
  )(
    v: => Alg[WithSource]
  )(
    expected: String
  )(
    implicit loc: SourceLocation
  ) =
    pureTest(label) {
      val result = Formatter[Alg].format(v, 80)
      // not using assertNoDiff because of
      // https://github.com/softwaremill/diffx/issues/422
      // which can result in false negatives (different strings considered equal and passing the test).
      assert.eql(result, expected) &&
      ParserSuite
        .assertParses(result)
        .fold(failure(_), _ => success)
    }

  def parse[Alg[_[_]]: SourceParser](s: String): Alg[WithSource] =
    SourceParser[Alg].parse(s).toTry.get

  formattingTest("struct: simple") {
    parse[Struct]("{ a : 42 }")
  }("""{
      |  a: 42,
      |}""".stripMargin)

  formattingTest("struct: nested struct") {
    parse[Struct]("{ a : { b : 42 } }")
  }("""{
      |  a: {
      |    b: 42,
      |  },
      |}""".stripMargin)

  formattingTest("struct: contains list") {
    parse[Struct]("{ a : [42] }")
  }("""{
      |  a: [
      |    42,
      |  ],
      |}""".stripMargin)

  formattingTest("list: newline is forced before multi-line comments") {
    parse[Listed]("""[//line 1
    //line 2
    10, 20]""".stripMargin)
  }("""[
      |  // line 1
      |  // line 2
      |  10,
      |  20,
      |]""".stripMargin)

  formattingTest("complex: string list with no comments") {
    "hello"
      .call(
        "values" -> List("hello", "world")
      )
      .mapK(WithSource.liftId)
  }("""hello {
      |  values: [
      |    "hello",
      |    "world",
      |  ],
      |}""".stripMargin)

  formattingTest("struct comments: before key") {
    parse[Struct]("""{ //this is a key
                    |input: 42}""".stripMargin)
  }("""{
      |  // this is a key
      |  input: 42,
      |}""".stripMargin)

  formattingTest("struct comments: before colon") {
    parse[Struct]("""{ input //this is a key
                    |: 42}""".stripMargin)
  }("""{
      |  input // this is a key
      |  : 42,
      |}""".stripMargin)

  formattingTest("struct comments: after colon") {
    parse[Struct]("""{ input : //this is a key
                    |42}""".stripMargin)
  }("""{
      |  input: // this is a key
      |    42,
      |}""".stripMargin)

  formattingTest("struct comments: after value") {
    parse[Struct]("""{ input: 42 //this is a value
                    |}""".stripMargin)
  }("""{
      |  input: 42 // this is a value
      |  ,
      |}""".stripMargin)

  formattingTest("list: no comments") {
    parse[Listed]("""[10, 11, 12]""")
  }("""[
      |  10,
      |  11,
      |  12,
      |]""".stripMargin)

  formattingTest("nested list: no comments") {
    parse[Listed]("""[[10, 11, 12]]""")
  }("""[
      |  [
      |    10,
      |    11,
      |    12,
      |  ],
      |]""".stripMargin)

  formattingTest("list comments: before entry") {
    parse[Listed]("""[ // hello
                    |42]""".stripMargin)
  }("""[
      |  // hello
      |  42,
      |]""".stripMargin)

  formattingTest("list comments: after entry") {
    parse[Listed]("""[42 // hello
                    |]""".stripMargin)
  }("""[
      |  42 // hello
      |  ,
      |]""".stripMargin)

  formattingTest("list comments: between entries") {
    parse[Listed]("""[42, // hello
                    |50]""".stripMargin)
  }("""[
      |  42,
      |  // hello
      |  50,
      |]""".stripMargin)

  formattingTest("list comments: after entries") {
    parse[Listed]("""[42, // hello
                    |50 // last
                    |]""".stripMargin)
  }("""[
      |  42,
      |  // hello
      |  50 // last
      |  ,
      |]""".stripMargin)

  formattingTest(
    "complex: struct with a list field that has a left-side comment - extra nesting is applied"
  ) {
    parse[Struct]("""{input: // comment before list
                    |[42, 10]
                    |}""".stripMargin)
  }("""{
      |  input: // comment before list
      |    [
      |      42,
      |      10,
      |    ],
      |}""".stripMargin)

  formattingTest(
    "complex: struct with a struct field that has a left-side comment - extra nesting is applied"
  ) {
    parse[Struct]("""{input: // comment before child
                    |{ hello: true }
                    |}""".stripMargin)
  }("""{
      |  input: // comment before child
      |    {
      |      hello: true,
      |    },
      |}""".stripMargin)

  formattingTest(
    "complex: struct with a list field that has a right-side comment"
  ) {
    parse[Struct]("""{input: [42, 10] // comment after list
                    |}""".stripMargin)
  }("""{
      |  input: [
      |    42,
      |    10,
      |  ] // comment after list
      |  ,
      |}""".stripMargin)

  formattingTest("complex: struct with int list with lots of comments") {
    parse[Struct]("""{ input :
      //this is a list
      [
        //list elems can be anything
        {} // and can have comments afterwards
        , //and between elems
        42 // and can have comments before ending
         //and also trailing commas
        ,
      ] // and can have comments after themselves
      ,
    }""")
  }(s"""{
       |  input: // this is a list
       |    [
       |      // list elems can be anything
       |      {
       |
       |      } // and can have comments afterwards
       |      ,
       |      // and between elems
       |      42
       |      // and can have comments before ending
       |      // and also trailing commas
       |      ,
       |    ] // and can have comments after themselves
       |  ,
       |}""".stripMargin)

  // formattingTest("use service clause with lots of comments") {
  //   parse[SourceFile]("""//before clause
  //   use service com.example#Service

  //   // after clause
  //   hello { }""")
  // }("""// before clause
  //     |use service com.example#Service
  //     |// after clause
  //     |
  //     |hello {
  //     |
  //     |}
  //     |""".stripMargin)

  // formattingTest("no service clause with comment on the call") {
  //   parse[SourceFile]("""//before call
  //   hello { }""")
  // }("""// before call
  //     |hello {
  //     |
  //     |}
  //     |""".stripMargin)

  // formattingTest("no service clause with comment on the call and explicit service ref") {
  //   parse[SourceFile]("""//before call
  //   a.b#C.hello { }""")
  // }("""// before call
  //     |a.b#C.hello {
  //     |
  //     |}
  //     |""".stripMargin)

  // formattingTest("multiple operations") {
  //   parse[SourceFile]("""Op {}
  //                       |Op {}""".stripMargin)
  // }("""Op {
  //     |
  //     |}
  //     |Op {
  //     |
  //     |}
  //     |""".stripMargin)

  // formattingTest("use service clause, then operation") {
  //   parse[SourceFile]("""use service a#B
  //                       |Op {}""".stripMargin)
  // }("""use service a#B
  //     |
  //     |Op {
  //     |
  //     |}
  //     |""".stripMargin)

  // formattingTest("use service clause, then multiple operations") {
  //   parse[SourceFile]("""use service a#B
  //                       |Op {}
  //                       |Op {}""".stripMargin)
  // }("""use service a#B
  //     |
  //     |Op {
  //     |
  //     |}
  //     |Op {
  //     |
  //     |}
  //     |""".stripMargin)

  // pureTest("Comments aren't lost when formatting") {
  //   val parsed = parse[SourceFile](Examples.fullOfComments)
  //   val result = Formatter[SourceFile]
  //     .format(parsed, 80)
  //     .pipe(parse[SourceFile])

  //   assertNoDiff(
  //     WithSource.allSourceComments(result),
  //     List(
  //       Comment(" before use clause"),
  //       Comment(" before op"),
  //       Comment(" after op"),
  //       Comment(" before key"),
  //       Comment(" after key"),
  //       Comment("  before value"),
  //       Comment("  after value"),
  //       Comment(" before another key"),
  //       Comment(" after second key"),
  //       Comment(" before value"),
  //       Comment(" after value"),
  //       Comment(" after trailing comma, technically this is part of the struct"),
  //       Comment("  after whole thing"),
  //     ),
  //   )
  // }

}
