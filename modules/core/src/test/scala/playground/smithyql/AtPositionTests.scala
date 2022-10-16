package playground.smithyql

import weaver._
import playground.smithyql.parser.SmithyQLParser
import playground.Assertions._
import playground.Diffs._

object AtPositionTests extends SimpleIOSuite {

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

  test("atPosition - 1 level deep") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = { ${CURSOR}mid = { child = "hello", }, }, }"""
    )

    assertNoDiff(
      actual,
      Some(
        NodeContext
          .Root
          .inOperationInput
          .inStructBody
          .inStructValue("root")
          .inStructBody
      ),
    )
  }

  test("atPosition - 2 levels deep") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = { mid = {${CURSOR} child = "hello", }, }, }"""
    )

    assertNoDiff(
      actual,
      Some(
        NodeContext
          .Root
          .inOperationInput
          .inStructBody
          .inStructValue("root")
          .inStructBody
          .inStructValue("mid")
          .inStructBody
      ),
    )
  }

  test("atPosition - on operation") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operat${CURSOR}ion { root = { mid = { child = "hello", }, }, }"""
    )

    assertNoDiff(
      actual,
      Some(
        NodeContext.Root.inOperationName
      ),
    )
  }

  test("atPosition - on list") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = ${CURSOR}[ { mid = { inner = "hello", }, } ],  }"""
    )

    val expected = NodeContext.Root.inOperationInput.inStructBody.inStructValue("root")

    assertNoDiff(
      actual,
      Some(
        expected
      ),
    )
  }

  test("atPosition - inside list") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = [ ${CURSOR} { mid = { inner = "hello", }, } ],  }"""
    )

    val expected = NodeContext
      .Root
      .inOperationInput
      .inStructBody
      .inStructValue("root")
      .inCollectionEntry(None)

    assertNoDiff(
      actual,
      Some(expected),
    )
  }

  test("atPosition - on item in list") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = [ { ${CURSOR} mid = { inner = "hello", }, } ],  }"""
    )

    val expected =
      NodeContext
        .Root
        .inOperationInput
        .inStructBody
        .inStructValue("root")
        .inCollectionEntry(Some(0))
        .inStructBody

    assertNoDiff(actual, Some(expected))
  }

  test("atPosition - on nested item in list") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = [ {}, { mid = { ${CURSOR} inner = "hello", }, } ],  }"""
    )

    assertNoDiff(
      actual,
      Some(
        NodeContext
          .Root
          .inOperationInput
          .inStructBody
          .inStructValue("root")
          .inCollectionEntry(Some(1))
          .inStructBody
          .inStructValue("mid")
          .inStructBody
      ),
    )
  }

  test("atPosition - around struct ") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = $CURSOR{ }, }"""
    )

    assertNoDiff(
      actual,
      Some(
        NodeContext.Root.inOperationInput.inStructBody.inStructValue("root")
      ),
    )
  }

  test("atPosition - in struct") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { root = {$CURSOR}, }"""
    )

    assertNoDiff(
      actual,
      Some(
        NodeContext.Root.inOperationInput.inStructBody.inStructValue("root").inStructBody
      ),
    )
  }

  test("atPosition - on field outside quotes") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { field = $CURSOR"", }"""
    )

    assertNoDiff(
      actual,
      Some(NodeContext.Root.inOperationInput.inStructBody.inStructValue("field")),
    )

  }

  test("atPosition - on string field in quotes") { (_, l) =>
    implicit val log = l
    val actual = locateAtCursor(
      s"""Operation { field = "$CURSOR", }"""
    )

    assertNoDiff(
      actual,
      Some(
        NodeContext.Root.inOperationInput.inStructBody.inStructValue("field").inQuotes
      ),
    )
  }
}
