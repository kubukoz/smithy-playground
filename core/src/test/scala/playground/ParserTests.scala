package playground

import munit.FunSuite
import munit.Location
import munit.TestOptions

class ParserTests extends FunSuite {

  def parsingTest(name: TestOptions, input: String)(expected: Query)(implicit loc: Location): Unit =
    test(name) {
      val actual = SmithyQLParser.parse(input)
      assertEquals(actual, Right(expected))
    }

  import DSL._

  parsingTest("simple call", "hello { } ")("hello".call())

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
    """More complicated case with indentation and comments""",
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
}
