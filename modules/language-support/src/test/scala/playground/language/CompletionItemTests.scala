package playground.language

import weaver._
import playground.std.ClockGen
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.syntax._
import playground.Assertions._
import Diffs._
import demo.smithy.Hero
import smithy4s.schema.Schema
import demo.smithy.Subscription

object CompletionItemTests extends FunSuite {
  test("CompletionItem.forOperation: no use clause") {
    val result = CompletionItem.forOperation(
      insertUseClause = CompletionItem.InsertUseClause.NotRequired,
      endpoint = ClockGen.CurrentTimestamp,
      serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
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

  test("CompletionItem.fromAlt: struct item") {
    val result = CompletionItem.fromAlt(
      Hero.GoodCase.alt.mapK(CompletionVisitor),
      Hero.GoodCase.schema,
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
      Hero.GoodCase.alt.mapK(CompletionVisitor),
      Schema.string,
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
