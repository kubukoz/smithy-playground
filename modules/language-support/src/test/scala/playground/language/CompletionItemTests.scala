package playground.language

import demo.smithy.CreateHeroInput
import demo.smithy.DemoServiceOperation.CreateHero
import demo.smithy.Good
import demo.smithy.Hero
import demo.smithy.Subscription
import playground.Assertions.*
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.syntax.*
import playground.std.ClockGen
import playground.std.ClockOperation
import smithy.api.Documentation
import smithy.api.Examples
import smithy4s.Hints
import smithy4s.schema.Schema
import weaver.*

import Diffs.given

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
      endpoint = ClockOperation.CurrentTimestamp,
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
      endpoint = ClockOperation.CurrentTimestamp,
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
      endpoint = ClockOperation.CurrentTimestamp,
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

  test("CompletionItem.forInputExamples: documentation already present is ignored") {
    val schema = CreateHeroInput
      .schema
      .addHints(CreateHero.hints.get(Examples).map(a => a: Hints.Binding).toList*)
      .addHints(Documentation("hello"))

    val results = CompletionItem.forInputExamples(schema)

    val containsDocs = results.exists(_.docs.exists(_.contains("hello")))

    expect(!containsDocs)
  }

  test("describeSchema: recursive struct") {
    val result = CompletionItem.describeSchema(Subscription.schema)()

    expect.eql(result, "structure Subscription")
  }
}
