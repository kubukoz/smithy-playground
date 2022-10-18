package playground.smithyql.format

import weaver._
import weaver.scalacheck.Checkers

import playground.smithyql.parser.Examples
import playground.smithyql._
import DSL._
import playground.smithyql.parser.SourceParser

object FormattingTests extends SimpleIOSuite with Checkers {

  def formattingTest(
    label: TestName
  )(
    q: => Query[WithSource]
  )(
    expected: String
  )(
    implicit loc: SourceLocation
  ) =
    pureTest(label) {
      val result = playground.smithyql.format.Formatter.format(q, 80)
      assert.eql(result, expected)
    }

  def parse(s: String): Query[WithSource] = SourceParser[Query].parse(s).toTry.get

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
    parse("""hello { input :
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
  }("""hello {
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
      |}
      |""".stripMargin)

  formattingTest("use service clause with lots of comments") {
    parse("""//before clause
    use service com.example#Service

    // after clause
    hello { }""")
  }("""// before clause
      |use service com.example#Service
      |// after clause
      |
      |hello {
      |
      |}
      |""".stripMargin)

  formattingTest("no service clause with comment on the call") {
    parse("""//before call
    hello { }""")
  }("""// before call
      |hello {
      |
      |}
      |""".stripMargin)

  formattingTest("no service clause with comment on the call and explicit service ref") {
    parse("""//before call
    a.b#C.hello { }""")
  }("""// before call
      |a.b#C.hello {
      |
      |}
      |""".stripMargin)

  pureTest("Comments aren't lost when formatting") {
    val result = SourceParser[Query]
      .parse(Examples.fullOfComments)
      .map(playground.smithyql.format.Formatter.format(_, 80))
      .flatMap(SourceParser[Query].parse)

    assert.eql(
      result.map(WithSource.allQueryComments),
      Right(
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
        )
      ),
    )
  }

}
