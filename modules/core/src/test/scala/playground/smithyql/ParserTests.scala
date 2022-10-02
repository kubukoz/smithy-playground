package playground.smithyql

import cats.Id
import playground.smithyql.Query
import weaver._
import cats.implicits._
import playground.Assertions._
import cats.effect.IO
import playground.smithyql.parser.SmithyQLParser

object ParserTests extends SimpleIOSuite {

  def parsingTest(
    name: TestName,
    input: String,
  )(
    expected: Query[Id]
  )(
    implicit loc: SourceLocation
  ): Unit =
    pureTest(name) {
      SmithyQLParser.parse(input) match {
        case Left(e)  => failure(s"Parsing failed: ${e.msg}")
        case Right(v) => assert(v == expected)
      }
    }

  def parsingTestFull(
    name: TestName,
    input: String,
  )(
    expected: Query[WithSource]
  )(
    implicit loc: SourceLocation
  ): Unit =
    test(name) { (_, l) =>
      implicit val log = l
      SmithyQLParser.parseFull(input) match {
        case Left(e)  => IO(failure(s"Parsing failed: ${e.msg}"))
        case Right(v) => assertNoDiff(v, expected)
      }
    }

  import DSL._

  parsingTestFull("simple call", "hello {}")(
    Query[WithSource](
      useClause = WithSource.liftValue(None),
      operationName = WithSource
        .liftValue(
          QueryOperationName(
            identifier = None,
            operationName = WithSource
              .liftValue(OperationName[WithSource]("hello"))
              .withRange(SourceRange(Position(0), Position(5))),
          )
        )
        .withRange(SourceRange(Position(0), Position(5))),
      input = WithSource
        .liftValue(
          Struct(
            WithSource
              .liftValue(Struct.Fields[WithSource](Nil))
              .withRange(SourceRange.empty(Position(7)))
          )
        )
        .withRange(SourceRange.empty(Position(7))),
    )
  )

  parsingTestFull("simple call, dense", "hello{}")(
    Query[WithSource](
      useClause = WithSource.liftValue(None),
      operationName = WithSource
        .liftValue(
          QueryOperationName(
            identifier = None,
            operationName = WithSource
              .liftValue(OperationName[WithSource]("hello"))
              .withRange(SourceRange(Position(0), Position(5))),
          )
        )
        .withRange(SourceRange(Position(0), Position(5))),
      input = WithSource
        .liftValue(
          Struct(
            WithSource
              .liftValue(Struct.Fields[WithSource](Nil))
              .withRange(SourceRange.empty(Position(6)))
          )
        )
        .withRange(SourceRange(Position(6), Position(6))),
    )
  )

  parsingTest("simple call, space in object", "hello{ }")("hello".call())
  parsingTest("simple call, trailing/leading space", " hello{} ")("hello".call())
  parsingTest("simple call, sparse", " hello { } ")("hello".call())
  parsingTest("simple call, sparse with underscore", " hello_world { } ")("hello_world".call())

  parsingTest("simple call with float", "hello { a = 21.37 }")("hello".call("a" -> 21.37f))
  parsingTest(
    "simple call with double",
    s"hello { a = ${Double.MaxValue.toString} }",
  )(
    "hello".call(
      "a" -> Double.MaxValue
    )
  )

  parsingTest("use service", "use service com.example#Demo hello {}")(
    "hello".call().useService("com", "example")("Demo")
  )

  parsingTest("use service with numbers", "use service com1.example2#Demo3 hello {}")(
    "hello".call().useService("com1", "example2")("Demo3")
  )

  parsingTest("use service with whitespace", "use service com1 . example2 # Demo3 hello {}")(
    "hello".call().useService("com1", "example2")("Demo3")
  )

  parsingTest("use service with underscore", "use service com.aws#Kinesis_2022 hello {}")(
    "hello".call().useService("com", "aws")("Kinesis_2022")
  )

  parsingTest("per-operation service reference", "com.aws#Kinesis.hello {}")(
    Query[Id](
      useClause = None,
      operationName = QueryOperationName[Id](
        QualifiedIdentifier.of("com", "aws", "Kinesis").some,
        OperationName("hello"),
      ),
      struct(),
    )
  )

  parsingTest(
    "per-operation service reference, with whitespace",
    "com . aws # Kinesis . hello {}",
  )(
    Query[Id](
      useClause = None,
      operationName = QueryOperationName[Id](
        QualifiedIdentifier.of("com", "aws", "Kinesis").some,
        OperationName("hello"),
      ),
      struct(),
    )
  )

  val simpleResult = "hello".call("world" -> "bar")

  parsingTest("one parameter single-line", """hello { world = "bar" }""")(
    simpleResult
  )

  parsingTest("one parameter single-line + trailing comma", """hello { world = "bar", }""")(
    simpleResult
  )

  parsingTest(
    "one parameter multiline",
    """
  hello {
    world = "bar"
  }
  """,
  )(simpleResult)

  parsingTest(
    "one parameter multiline + trailing comma",
    """
  hello {
    world = "bar",
  }
  """,
  )(simpleResult)

  parsingTest(
    "struct with comment and trailing comma",
    """
  hello {
    world = "bar",
    //
  }
  """,
  )(
    "hello".call(
      "world" -> "bar"
    )
  )

  parsingTest(
    "empty struct",
    """hello {
    //hello
  }""",
  )("hello".call())

  parsingTest(
    "one parameter multiline split",
    """ hello {
    world
    =
    "bar"
    }
  }""",
  )

  parsingTest("struct with bool", "hello { verbose = true }")(
    "hello".call(
      "verbose" -> true
    )
  )

  parsingTest("list with bools", "hello { values = [true, false] }")(
    "hello".call(
      "values" -> List(true, false)
    )
  )

  parsingTest("list with bools, trailing comma", "hello { values = [true, false,] }")(
    "hello".call(
      "values" -> List(true, false)
    )
  )

  parsingTest("list with strings", """hello { values = ["hello", "world",] }""")(
    "hello".call(
      "values" -> List("hello", "world")
    )
  )

  parsingTest("list with ints", """hello { values = [420, 2137,] }""")(
    "hello".call(
      "values" -> List(420, 2137)
    )
  )

  parsingTest(
    "list with comments",
    """
    hello { values =
         //before list
     [
         //before first elem
     420 //after first elem
     ,   //before second elem

      2137 //after second elem,
           //after trailing comma
      ]    //after list
    }""",
  )(
    "hello".call(
      "values" -> List(420, 2137)
    )
  )

  parsingTest(
    "empty comment",
    "//\nhello{}",
  )("hello".call())

  parsingTest(
    "nonempty comment",
    """hello {
    // hello
    }
    """,
  )("hello".call())

  parsingTest(
    "comment inside line",
    """
  hello //foo
    {
    world = "bar"
    }
    """,
  )(simpleResult)

  parsingTest(
    "comment at line start",
    """
  hello
  //this is a comment
  {
    world = "bar"
    }
    """,
  )(simpleResult)

  parsingTest(
    "multiple line comments",
    """
  hello {
  //this is a comment
  //this too
    world = "bar"
  }""",
  )(simpleResult)

  parsingTest(
    "more line comments",
    """
  hello {
  //this is a comment
  world = "bar"
  //this too
  }""",
  )(simpleResult)

  parsingTest(
    "comment in the middle of a keypair before line break",
    """
  hello {
  world = //this is a comment
     "bar"
  }""",
  )(simpleResult)

  parsingTest(
    "comment in the middle of a keypair after line break",
    """
  hello {
  world =
  //this is a comment
     "bar"
  }""",
  )(simpleResult)

  parsingTest(
    "more comments in the middle of a keypair",
    """
  hello {
  world = //this is a comment
  //this too
     "bar"
  }""",
  )(simpleResult)

  parsingTest(
    "more comments in the middle of a keypair, with preceding whitespace",
    """
  hello {
  world = //this is a comment
          //this too
     "bar"
  }""",
  )(simpleResult)

  parsingTest(
    "More complicated case with indentation and comments",
    """
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
""",
  )(
    "CreateHero".call(
      "hero" -> struct("good" -> struct("howGood" -> 200, "anotherKey" -> 42))
    )
  )

  parsingTest(
    "Comments literally everywhere",
    Examples.fullOfComments,
  )(
    "op"
      .call(
        "firstKey" -> "firstValue",
        "secondKey" -> "secondValue",
      )
      .useService("some", "api")("Service")
  )
}
