package playground.smithyql

import weaver._
import weaver.scalacheck.Checkers

import DSL._

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
      val result = playground.smithyql.Formatter.format(q, 80)
      assert.eql(result, expected)
    }

  def parse(s: String): Query[WithSource] = SmithyQLParser.parseFull(s).toTry.get

  formattingTest("string list with no comments") {
    "hello"
      .call(
        "values" -> List("hello", "world")
      )
      .mapK(WithSource.liftId)
  }("""hello {
      |  values = [
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
      |  values = [
      |       42,
      |       10,
      |    ],
      |}
      |""".stripMargin)

  formattingTest("int list with lots of comments") {
    parse("""hello { input =
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
      |  input = // this is a list
      |    [
      |       // list elems can be anything
      |      {
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
      |
      |// after clause
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
}
