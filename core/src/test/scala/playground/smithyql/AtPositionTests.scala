package playground.smithyql

import weaver._

object AtPositionTests extends FunSuite {

  val CURSOR = """<<HERE>>"""

  def extractCursor(s: String): (String, Position) = {
    val cursor = Position(s.indexOf(CURSOR))

    (s.replace(CURSOR, ""), cursor)
  }

  def locateAtCursor(text: String) = {
    val (extracted, position) = extractCursor(text)
    val parsed =
      SmithyQLParser
        .parseFull(extracted)
        .toTry
        .get

    WithSource.atPosition(parsed)(position)
  }

  test("atPosition - 1 level deep") {
    val actual = locateAtCursor(
      s"""Operation { root = { ${CURSOR}mid = { child = "hello", }, }, }"""
    )

    assert(actual == Some(WithSource.NodeContext.InputContext("root" :: Nil)))
  }

  test("atPosition - 2 levels deep") {
    val actual = locateAtCursor(
      s"""Operation { root = { mid = {${CURSOR} child = "hello", }, }, }"""
    )

    assert(actual == Some(WithSource.NodeContext.InputContext("root" :: "mid" :: Nil)))
  }

  test("atPosition - on operation") {
    val actual = locateAtCursor(
      s"""Operat${CURSOR}ion { root = { mid = { child = "hello", }, }, }"""
    )

    assert(
      actual == Some(
        WithSource
          .NodeContext
          .OperationContext(
            WithSource(
              commentsLeft = Nil,
              commentsRight = Nil,
              range = SourceRange(Position(0), Position("Operation".length)),
              value = OperationName("Operation"),
            )
          )
      )
    )
  }

  test("atPosition - on list".only) {
    val actual = locateAtCursor(
      s"""Operation { root = [ ${CURSOR} { mid = { inner = "hello", }, } ],  }"""
    )

    assert(actual == Some(WithSource.NodeContext.InputContext("root" :: Nil)))
  }

  test("atPosition - on item in list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ { ${CURSOR} mid = { inner = "hello", }, } ],  }"""
    )

    assert(actual == Some(WithSource.NodeContext.InputContext("root" :: Nil)))
  }

  test("atPosition - on nested item in list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ { mid = { ${CURSOR} inner = "hello", }, } ],  }"""
    )

    assert(actual == Some(WithSource.NodeContext.InputContext("root" :: "mid" :: Nil)))
  }
}
