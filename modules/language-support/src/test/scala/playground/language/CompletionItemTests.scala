package playground.language

import weaver._
import playground.std.ClockGen
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.syntax._
import playground.Assertions._
import Diffs._
import demo.smithy.Hero

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

  test("CompletionItem.fromAlt") {
    val result = CompletionItem.fromAlt(
      Hero.GoodCase.alt.mapK(CompletionVisitor),
      Hero.GoodCase.schema,
    )

    assertNoDiff(
      result,
      CompletionItem(
        kind = CompletionItemKind.UnionMember,
        label = "good",
        insertText = InsertText.SnippetString("good: {$0},"),
        detail = ": structure Good",
        description = Some("demo.smithy"),
        deprecated = false,
        docs = None,
        extraTextEdits = Nil,
        sortText = None,
      ),
    )
  }
}
