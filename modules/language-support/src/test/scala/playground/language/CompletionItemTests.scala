package playground.language

import demo.smithy.Hero
import demo.smithy.Subscription
import playground.Assertions._
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.syntax._
import playground.std.ClockGen
import smithy4s.schema.Schema
import weaver._

import Diffs._

object CompletionItemTests extends FunSuite {
  test("CompletionItem.fromField: required field") {
    val result = CompletionItem.fromField(
      Schema.string.required[String]("test", identity(_)).addHints(smithy.api.Required())
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.Field,
        label = "test",
        insertText = InsertText.JustString("test: "),
        detail = ": string String",
        description = Some("smithy.api"),
        deprecated = false,
        docs = None,
        extraTextEdits = Nil,
        sortText = Some("1_test"),
      ),
    )
  }

  test("CompletionItem.fromField: optional field") {
    val result = CompletionItem.fromField(
      Schema.string.optional[Option[String]]("test", identity(_))
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.Field,
        label = "test",
        insertText = InsertText.JustString("test: "),
        detail = "?: string String",
        description = Some("smithy.api"),
        deprecated = false,
        docs = Some("**Optional**"),
        extraTextEdits = Nil,
        sortText = Some("2_test"),
      ),
    )
  }

  test("CompletionItem.forOperation: no use clause") {
    val result = CompletionItem.forOperation(
      insertUseClause = CompletionItem.InsertUseClause.NotRequired,
      endpoint = ClockGen.CurrentTimestamp,
      serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
      CompletionItem.InsertBodyStruct.Yes,
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.Function,
        label = "CurrentTimestamp",
        insertText = InsertText.SnippetString("""CurrentTimestamp {
                                                |  $0
                                                |}""".stripMargin),
        detail = ": Unit => CurrentTimestampOutput",
        description = None,
        deprecated = false,
        docs = Some("Provides the current time as a Timestamp."),
        extraTextEdits = Nil,
        sortText = Some("1_CurrentTimestamp"),
      ),
    )
  }

  test("CompletionItem.forOperation: insert use clause") {
    val result = CompletionItem.forOperation(
      insertUseClause = CompletionItem.InsertUseClause.Required,
      endpoint = ClockGen.CurrentTimestamp,
      serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
      CompletionItem.InsertBodyStruct.Yes,
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.Function,
        label = "CurrentTimestamp",
        insertText = InsertText.SnippetString("""CurrentTimestamp {
                                                |  $0
                                                |}""".stripMargin),
        detail = "(from playground.std#Clock): Unit => CurrentTimestampOutput",
        description = None,
        deprecated = false,
        docs = Some("Provides the current time as a Timestamp."),
        extraTextEdits = List(
          TextEdit.Insert("use service playground.std#Clock\n\n", Position.origin)
        ),
        sortText = Some("2_CurrentTimestamp"),
      ),
    )
  }

  test("CompletionItem.forOperation: no struct body") {
    val result = CompletionItem.forOperation(
      insertUseClause = CompletionItem.InsertUseClause.NotRequired,
      endpoint = ClockGen.CurrentTimestamp,
      serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
      CompletionItem.InsertBodyStruct.No,
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.Function,
        label = "CurrentTimestamp",
        insertText = InsertText.JustString("CurrentTimestamp"),
        detail = ": Unit => CurrentTimestampOutput",
        description = None,
        deprecated = false,
        docs = Some("Provides the current time as a Timestamp."),
        extraTextEdits = Nil,
        sortText = Some("1_CurrentTimestamp"),
      ),
    )
  }

  test("CompletionItem.fromAlt: struct item") {
    val result = CompletionItem.fromAlt(
      Hero.GoodCase.alt
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.UnionMember,
        label = "good",
        insertText = InsertText.SnippetString("""good: {
                                                |  $0
                                                |},""".stripMargin),
        detail = ": structure Good",
        description = Some("demo.smithy"),
        deprecated = false,
        docs = None,
        extraTextEdits = Nil,
        sortText = None,
      ),
    )
  }

  test("CompletionItem.fromAlt: non-struct item") {
    val result = CompletionItem.fromAlt(
      Schema.string.oneOf[String]("good")
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.UnionMember,
        label = "good",
        insertText = InsertText.JustString("good: "),
        detail = ": string String",
        description = Some("smithy.api"),
        deprecated = false,
        docs = None,
        extraTextEdits = Nil,
        sortText = None,
      ),
    )
  }

  test("describeSchema: recursive struct") {
    val result = CompletionItem.describeSchema(Subscription.schema)()

    assert.eql(result, "structure Subscription")
  }
}
