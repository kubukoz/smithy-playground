package playground.smithyql

import weaver._
import cats.data.Chain
import NodeContext.PathEntry._

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

    RangeIndex
      .build(parsed)
      .findAtPosition(position)
      .map(_.ctx)
  }

  def assertFound(actual: Option[NodeContext], expected: NodeContext) =
    assert(actual == Some(expected)) &&
      assert.eql(actual.map(_.render), Some(expected.render))

  test("atPosition - 1 level deep") {
    val actual = locateAtCursor(
      s"""Operation { root = { ${CURSOR}mid = { child = "hello", }, }, }"""
    )

    assert(
      actual == Some(
        NodeContext
          .InputContext(
            Chain(
              StructBody,
              StructValue("root"),
              StructBody,
            )
          )
      )
    )
  }

  test("atPosition - 2 levels deep") {
    val actual = locateAtCursor(
      s"""Operation { root = { mid = {${CURSOR} child = "hello", }, }, }"""
    )

    assert(
      actual == Some(
        NodeContext
          .InputContext(
            Chain(
              StructBody,
              StructValue("root"),
              StructBody,
              StructValue("mid"),
              StructBody,
            )
          )
      )
    )
  }

  test("atPosition - on operation") {
    val actual = locateAtCursor(
      s"""Operat${CURSOR}ion { root = { mid = { child = "hello", }, }, }"""
    )

    val op = WithSource(
      commentsLeft = Nil,
      commentsRight = Nil,
      range = SourceRange(Position(0), Position("Operation".length)),
      value = OperationName("Operation"),
    )

    assert(
      actual == Some(
        NodeContext
          .OperationContext(
            op
          )
      )
    )
  }

  test("atPosition - on list") {
    val actual = locateAtCursor(
      s"""Operation { root = ${CURSOR}[ { mid = { inner = "hello", }, } ],  }"""
    )

    val expected = NodeContext.InputContext(
      Chain(
        StructBody,
        StructValue("root"),
      )
    )

    assert(
      actual == Some(
        expected
      )
    )
  }

  test("atPosition - inside list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ ${CURSOR} { mid = { inner = "hello", }, } ],  }"""
    )

    val expected = NodeContext
      .InputContext(
        Chain(
          StructBody,
          StructValue("root"),
          CollectionEntry(None),
        )
      )

    assert(
      actual == Some(
        expected
      )
    )
  }

  test("atPosition - on item in list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ { ${CURSOR} mid = { inner = "hello", }, } ],  }"""
    )

    val expected = NodeContext
      .InputContext(
        Chain(
          StructBody,
          StructValue("root"),
          CollectionEntry(Some(0)),
          StructBody,
        )
      )
    assert(actual == Some(expected))
  }

  test("atPosition - on nested item in list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ {}, { mid = { ${CURSOR} inner = "hello", }, } ],  }"""
    )

    assert(
      actual == Some(
        NodeContext
          .InputContext(
            Chain(
              StructBody,
              StructValue("root"),
              CollectionEntry(Some(1)),
              StructBody,
              StructValue("mid"),
              StructBody,
            )
          )
      )
    )
  }

  test("atPosition - around struct ") {
    val actual = locateAtCursor(
      s"""Operation { root = $CURSOR{ }, }"""
    )

    assert(
      actual == Some(
        NodeContext
          .InputContext(
            Chain(
              StructBody,
              StructValue("root"),
            )
          )
      )
    )
  }

  test("atPosition - in struct") {
    val actual = locateAtCursor(
      s"""Operation { root = {$CURSOR}, }"""
    )

    assert(
      actual == Some(
        NodeContext
          .InputContext(
            Chain(
              StructBody,
              StructValue("root"),
              StructBody,
            )
          )
      )
    )
  }

  test("atPosition - on field outside quotes") {
    val actual = locateAtCursor(
      s"""Operation { field = $CURSOR"", }"""
    )

    assertFound(
      actual,
      NodeContext
        .InputContext(
          Chain(
            StructBody,
            StructValue("field"),
          )
        ),
    )

  }

  test("atPosition - on string field in quotes") {
    val actual = locateAtCursor(
      s"""Operation { field = "$CURSOR", }"""
    )

    assert(
      actual == Some(
        NodeContext
          .InputContext(
            Chain(
              StructBody,
              StructValue("field"),
              Quotes,
            )
          )
      )
    )
  }
}
