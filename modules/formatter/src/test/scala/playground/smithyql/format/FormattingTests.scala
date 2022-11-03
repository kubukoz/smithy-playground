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

object FormattingTests extends SimpleIOSuite with Checkers {

  def formattingTest[Alg[_[_]]: Formatter](
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
      assert.eql(result, expected)
    }

  def parse[Alg[_[_]]: SourceParser](s: String): Alg[WithSource] =
    SourceParser[Alg].parse(s).toTry.get

  formattingTest("string list with no comments") {
    "hello"
      .call(
        "values" -> List("hello", "world")
      )
      .mapK(WithSource.liftId)
  }("""hello {
      |  values: [
      |       "hello",
      |       "world",
      |    ],
      |}
      |""".stripMargin)

  formattingTest("int list with no comments") {
    "hello"
      .call(
        "values" -> List(42, 10)
      )
      .mapK(WithSource.liftId)
  }("""hello {
      |  values: [
      |       42,
      |       10,
      |    ],
      |}
      |""".stripMargin)

  formattingTest("int list with lots of comments") {
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
  }("""{
      |  input: // this is a list
      |    [
      |      // list elems can be anything
      |       {
      |
      |      }
      |      // and can have comments afterwards
      |      ,
      |       // and between elems
      |        42
      |        // and can have comments before ending
      |        // and also trailing commas
      |        ,
      |    ] // and can have comments after themselves
      |    ,
      |}""".stripMargin)

  formattingTest("use service clause with lots of comments") {
    parse[SourceFile]("""//before clause
    use service com.example#Service

    // after clause
    hello { }""")
  }("""// before clause
      |use service com.example#Service
      |// after clause
      |hello {
      |
      |}
      |""".stripMargin)

  formattingTest("no service clause with comment on the call") {
    parse[SourceFile]("""//before call
    hello { }""")
  }("""// before call
      |hello {
      |
      |}
      |""".stripMargin)

  formattingTest("no service clause with comment on the call and explicit service ref") {
    parse[SourceFile]("""//before call
    a.b#C.hello { }""")
  }("""// before call
      |a.b#C.hello {
      |
      |}
      |""".stripMargin)

  formattingTest("use service clause, then operation") {
    parse[SourceFile]("""use service a#B
                        |Op {}""".stripMargin)
  }("""use service a#B
      |Op {
      |
      |}
      |""".stripMargin)

  pureTest("Comments aren't lost when formatting") {
    val parsed = parse[SourceFile](Examples.fullOfComments)
    val result = Formatter[SourceFile]
      .format(parsed, 80)
      .pipe(parse[SourceFile])

    assertNoDiff(
      WithSource.allSourceComments(result),
      List(
        Comment(" before use clause"),
        Comment(" before op"),
        Comment(" after op"),
        Comment(" before key"),
        Comment(" after key"),
        Comment("  before value"),
        Comment("  after value"),
        Comment(" before another key"),
        Comment(" after second key"),
        Comment(" before value"),
        Comment(" after value"),
        Comment(" after trailing comma, technically this is part of the struct"),
        Comment("  after whole thing"),
      ),
    )
  }

}
