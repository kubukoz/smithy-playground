package playground.smithyql

import munit.FunSuite
import munit.Location
import munit.TestOptions
import playground.smithyql.Query
import cats.Id

class ParserTests extends FunSuite {

  def parsingTest(
    name: TestOptions,
    input: String,
  )(
    expected: Query[Id]
  )(
    implicit loc: Location
  ): Unit =
    test(name) {
      SmithyQLParser.parse(input) match {
        case Left(e)  => fail(s"Parsing failed: ${e.msg}")
        case Right(v) => assertEquals(v, expected)
      }
    }

  import DSL._

  parsingTest("simple call, dense", "hello{}")("hello".call())
  parsingTest("simple call, space in object", "hello{ }")("hello".call())
  parsingTest("simple call, trailing/leading space", " hello{} ")("hello".call())
  parsingTest("simple call, sparse", " hello { } ")("hello".call())

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
    "op".call(
      "firstKey" -> "firstValue",
      "secondKey" -> "secondValue",
    )
  )
}
