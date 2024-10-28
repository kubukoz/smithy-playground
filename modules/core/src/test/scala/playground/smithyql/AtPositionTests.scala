package playground.smithyql

import playground.Assertions._
import playground.Diffs.given
import playground.smithyql.parser.SourceParser
import weaver.*

object AtPositionTests extends FunSuite {

  val CURSOR = """<<HERE>>"""

  def extractCursor(
    s: String
  ): (
    String,
    Position,
  ) = {
    val cursor = Position(s.indexOf(CURSOR))

    (s.replace(CURSOR, ""), cursor)
  }

  def locateAtCursor(
    text: String
  ): NodeContext = {
    val (extracted, position) = extractCursor(text)
    val parsed =
      SourceParser[SourceFile]
        .parse(extracted)
        .toTry
        .get

    RangeIndex
      .build(parsed)
      .findAtPosition(position)
  }

  // tests for before/after/between queries
  // https://github.com/kubukoz/smithy-playground/issues/165

  private val firstOp = NodeContext.EmptyPath.inQuery(0)

  test("atPosition - empty file") {
    assertNoDiff(locateAtCursor(CURSOR), NodeContext.EmptyPath)
  }

  test("atPosition - in first use clause") {
    assertNoDiff(
      locateAtCursor(s"""use service ${CURSOR}a#B
                        |use service a#C""".stripMargin),
      NodeContext.EmptyPath.inPrelude.inUseClause(0),
    )
  }

  test("atPosition - in second use clause") {
    assertNoDiff(
      locateAtCursor(s"""use service a#B
                        |use service ${CURSOR}a#C""".stripMargin),
      NodeContext.EmptyPath.inPrelude.inUseClause(1),
    )
  }

  test("atPosition - in between use clauses") {
    assertNoDiff(
      locateAtCursor(
        s"""use service a#B
           |$CURSOR
           |use service a#C""".stripMargin
      ),
      NodeContext.EmptyPath.inPrelude,
    )
  }

  test("atPosition - 1 level deep") {
    val actual = locateAtCursor(
      s"""Operation { root = { ${CURSOR}mid = { child = "hello", }, }, }"""
    )

    assertNoDiff(
      actual,
      firstOp
        .inOperationInput
        .inStructBody
        .inStructValue("root")
        .inStructBody,
    )
  }

  test("atPosition - second query") {
    val actual = locateAtCursor(s"""op1 {}
                                   |op2 $CURSOR {}""".stripMargin)

    assertNoDiff(actual, NodeContext.EmptyPath.inQuery(1))
  }

  test("atPosition - between queries") {
    val actual = locateAtCursor(s"""op1 {}
                                   |
                                   |$CURSOR
                                   |
                                   |op2 {}""".stripMargin)

    assertNoDiff(actual, NodeContext.EmptyPath)
  }

  test("atPosition - second query op name") {
    val actual = locateAtCursor(s"""op1 {}
                                   |op${CURSOR}2 {}""".stripMargin)

    assertNoDiff(actual, NodeContext.EmptyPath.inQuery(1).inOperationName)
  }

  test("atPosition - 2 levels deep") {
    val actual = locateAtCursor(
      s"""Operation { root = { mid = {$CURSOR child = "hello", }, }, }"""
    )

    assertNoDiff(
      actual,
      firstOp
        .inOperationInput
        .inStructBody
        .inStructValue("root")
        .inStructBody
        .inStructValue("mid")
        .inStructBody,
    )
  }

  test("atPosition - on operation") {
    val actual = locateAtCursor(
      s"""Operat${CURSOR}ion { root = { mid = { child = "hello", }, }, }"""
    )

    assertNoDiff(
      actual,
      firstOp.inOperationName,
    )
  }

  test("atPosition - on list") {
    val actual = locateAtCursor(
      s"""Operation { root = $CURSOR[ { mid = { inner = "hello", }, } ],  }"""
    )

    val expected = firstOp.inOperationInput.inStructBody.inStructValue("root")

    assertNoDiff(
      actual,
      expected,
    )
  }

  test("atPosition - inside list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ $CURSOR { mid = { inner = "hello", }, } ],  }"""
    )

    val expected = firstOp
      .inOperationInput
      .inStructBody
      .inStructValue("root")
      .inCollectionEntry(None)

    assertNoDiff(
      actual,
      expected,
    )
  }

  test("atPosition - on item in list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ { $CURSOR mid = { inner = "hello", }, } ],  }"""
    )

    val expected =
      firstOp
        .inOperationInput
        .inStructBody
        .inStructValue("root")
        .inCollectionEntry(Some(0))
        .inStructBody

    assertNoDiff(actual, expected)
  }

  test("atPosition - on nested item in list") {
    val actual = locateAtCursor(
      s"""Operation { root = [ {}, { mid = { $CURSOR inner = "hello", }, } ],  }"""
    )

    assertNoDiff(
      actual,
      firstOp
        .inOperationInput
        .inStructBody
        .inStructValue("root")
        .inCollectionEntry(Some(1))
        .inStructBody
        .inStructValue("mid")
        .inStructBody,
    )
  }

  test("atPosition - around struct ") {
    val actual = locateAtCursor(
      s"""Operation { root = $CURSOR{ }, }"""
    )

    assertNoDiff(
      actual,
      firstOp.inOperationInput.inStructBody.inStructValue("root"),
    )
  }

  test("atPosition - in struct") {
    val actual = locateAtCursor(
      s"""Operation { root = {$CURSOR}, }"""
    )

    assertNoDiff(
      actual,
      firstOp.inOperationInput.inStructBody.inStructValue("root").inStructBody,
    )
  }

  test("atPosition - on field outside quotes") {
    val actual = locateAtCursor(
      s"""Operation { field = $CURSOR"", }"""
    )

    assertNoDiff(
      actual,
      firstOp.inOperationInput.inStructBody.inStructValue("field"),
    )

  }

  test("atPosition - on string field in quotes") {
    val actual = locateAtCursor(
      s"""Operation { field = "$CURSOR", }"""
    )

    assertNoDiff(
      actual,
      firstOp.inOperationInput.inStructBody.inStructValue("field").inQuotes,
    )
  }
}
