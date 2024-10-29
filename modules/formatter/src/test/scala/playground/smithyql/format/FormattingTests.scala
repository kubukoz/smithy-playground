package playground.smithyql.format

import cats.syntax.all.*
import playground.Assertions.*
import playground.smithyql.*
import playground.smithyql.format.Formatter
import playground.smithyql.parser.Examples
import playground.smithyql.parser.ParserSuite
import playground.smithyql.parser.SourceParser
import util.chaining.*
import weaver.*
import weaver.scalacheck.Checkers

import Diffs.given

object FormattingTests extends SimpleIOSuite with Checkers {

  def formattingTest[Alg[_[_]]: Formatter: SourceParser](
    label: TestName
  )(
    v: => Alg[WithSource]
  )(
    expected: String
  )(
    implicit loc: SourceLocation
  ): Unit =
    pureTest(label) {
      val result = Formatter[Alg].format(v, 80)
      // not using assertNoDiff because of
      // https://github.com/softwaremill/diffx/issues/422
      // which can result in false negatives (different strings considered equal and passing the test).
      assert.eql(result, expected).traced(SourceLocation.fromContext) &&
      ParserSuite
        .assertParses(result)
        .fold(failure(_), _ => success)
    }

  def parse[Alg[_[_]]: SourceParser](
    s: String
  ): Alg[WithSource] = SourceParser[Alg].parse(s).leftMap(_.debug).fold(sys.error(_), identity)

  formattingTest("struct: empty") {
    parse[Struct]("{}")
  }("""{
      |
      |}""".stripMargin)
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

  formattingTest("struct comments: middle value commented out") {
    parse[Struct]("""{
                    |  input: 42,
                    |  //input2: 43,
                    |  input3: 44
                    |}""".stripMargin)
  }(
    // this works that way because the comment is seen as a LHS comment of the input3 field node.
    """{
      |  input: 42,
      |  // input2: 43,
      |  input3: 44,
      |}""".stripMargin
  )

  formattingTest("struct comments: last value commented out") {
    parse[Struct]("""{
                    |  input: 42,
                    |  //input2: 43,
                    |}""".stripMargin)
  }(
    // this shows up like that because input2 is seen as a RHS comment on the `fields` node
    """{
      |  input: 42, // input2: 43,
      |}""".stripMargin
  )

  formattingTest("struct comments: last values (plural) commented out") {
    parse[Struct]("""{
                    |  input: 42,
                    |  //input2: 43,
                    |  //input3: 44
                    |}""".stripMargin)
  }(
    // the two fields are seen as the RHS of the `fields` node
    """{
      |  input: 42,
      |  // input2: 43,
      |  // input3: 44
      |}""".stripMargin
  )

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

  formattingTest("list: commented out first item") {
    parse[Listed]("""[
                    |// 10,
                    |  11,
                    |]""".stripMargin)
  }("""[
      |  // 10,
      |  11,
      |]""".stripMargin)

  formattingTest("list: commented out item") {
    parse[Listed]("""[
                    |10,
                    |// 11,
                    |]""".stripMargin)
  }("""[
      |  10, // 11,
      |]""".stripMargin)

  formattingTest("list: commented out items (plural)") {
    parse[Listed]("""[
                    |10,
                    |// 11,
                    |// 12,
                    |]""".stripMargin)
  }("""[
      |  10,
      |  // 11,
      |  // 12,
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

  formattingTest("use service clause with whitespace in it") {
    parse[UseClause]("""use
    service
    com . example # Service""")
  }("""use service com.example#Service""".stripMargin)

  formattingTest("use service clause with no whitespace") {
    parse[UseClause]("""use service com.example#Service""")
  }("""use service com.example#Service""".stripMargin)

  // parser test & fix: use clauses MUST have whitespace between keywords
  // https://github.com/kubukoz/smithy-playground/issues/160
  formattingTest("prelude with one use clause") {
    parse[Prelude]("""use service com.example#Service""")
  }("""use service com.example#Service""".stripMargin)

  formattingTest("prelude with one use clause, comments before") {
    parse[Prelude]("""// hello
    use service com.example#Service""")
  }("""// hello
      |use service com.example#Service""".stripMargin)

  formattingTest("prelude with multiple use clauses") {
    parse[Prelude]("""use service a#B
                     |use service c#D""".stripMargin)
  }("""use service a#B
      |use service c#D""".stripMargin)

  formattingTest("prelude with multiple use clauses, comments") {
    parse[Prelude]("""// before
                     |use service a#B // between
                     |use service c#D""".stripMargin)
  }(
    // N.B. this is the result of only parsing LHS comments in use clauses.
    // Not purely accidental, but also not eagerly intended.
    """// before
      |use service a#B
      |// between
      |use service c#D""".stripMargin
  )

  // comments are illegal after a prelude clause because it ends the prelude block

  formattingTest("empty prelude") {
    parse[Prelude]("")
  }("")

  formattingTest("query operation name: just name") {
    parse[QueryOperationName]("hello")
  }("hello")

  formattingTest("query operation name: fully qualified name") {
    parse[QueryOperationName]("com.hello#Service.Op")
  }("com.hello#Service.Op")

  formattingTest("query operation name: fully qualified name with spaces") {
    parse[QueryOperationName]("com . hello # Service . Op")
  }("com.hello#Service.Op")

  formattingTest("query: simple, packed") {
    parse[Query]("hello{}")
  }("""hello {
      |
      |}""".stripMargin)

  formattingTest("query: simple, unpacked") {
    parse[Query]("hello { }")
  }("""hello {
      |
      |}""".stripMargin)

  formattingTest("query: comment between op name and input") {
    parse[Query]("""hello //a
                   |{}""".stripMargin)
  }("""hello // a
      |{
      |
      |}""".stripMargin)

  formattingTest("file made of just comments") {
    parse[SourceFile]("//hello")
  }("""// hello""".stripMargin)

  formattingTest("file: single query with comment on the call") {
    parse[SourceFile]("""//before call
                        |hello { }""".stripMargin)
  }("""// before call
      |hello {
      |
      |}""".stripMargin)

  formattingTest("file: single query with use clause") {
    parse[SourceFile]("""use service com#Hello
                        |hello { }""".stripMargin)
  }("""use service com#Hello
      |
      |hello {
      |
      |}""".stripMargin)

  formattingTest("multiple operations") {
    parse[SourceFile]("""Op {}
                        |Op2 {}""".stripMargin)
  }("""Op {
      |
      |}
      |
      |Op2 {
      |
      |}""".stripMargin)

  formattingTest("use service clause, then multiple operations") {
    parse[SourceFile]("""use service a#B
                        |Op {}
                        |Op2 {}""".stripMargin)
  }("""use service a#B
      |
      |Op {
      |
      |}
      |
      |Op2 {
      |
      |}""".stripMargin)

  formattingTest("multiple use service clauses, then operation") {
    parse[SourceFile]("""use service a#B
                        |use service c#D
                        |Op {}""".stripMargin)
  }("""use service a#B
      |use service c#D
      |
      |Op {
      |
      |}""".stripMargin)

  formattingTest("sorting use clauses") {
    parse[SourceFile]("""use service secondNamespace#Second
                        |use service firstNamespace#First
                        |use service secondNamespace#First
                        |use service firstNamespace#Second""".stripMargin)
  }("""use service firstNamespace#First
      |use service firstNamespace#Second
      |use service secondNamespace#First
      |use service secondNamespace#Second""".stripMargin)

  formattingTest("sorting use clauses - duplicates aren't removed") {
    parse[SourceFile]("""use service secondNamespace#First
                        |use service firstNamespace#First
                        |use service firstNamespace#First""".stripMargin)
  }("""use service firstNamespace#First
      |use service firstNamespace#First
      |use service secondNamespace#First""".stripMargin)

  formattingTest("use service clause, then comments and multiple operations") {
    parse[SourceFile]("""// before service
                        |use service a#B
                        |// before op
                        |Op {}
                        |// before op2
                        |Op2 {}
                        |// after op2""".stripMargin)
  }("""// before service
      |use service a#B
      |
      |// before op
      |Op {
      |
      |} // before op2
      |
      |Op2 {
      |
      |} // after op2""".stripMargin)

  pureTest("Comments aren't lost when formatting") {
    val parsed = parse[SourceFile](Examples.fullOfComments)
    val result = Formatter[SourceFile]
      .format(parsed, 80)
      .pipe(parse[SourceFile])

    assertNoDiff(
      WithSource.allSourceComments(result),
      List(
        Comment(" before use clause"),
        Comment(" before another clause"),
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
