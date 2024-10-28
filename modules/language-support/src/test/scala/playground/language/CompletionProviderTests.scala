package playground.language

import demo.smithy.DemoServiceGen
import demo.smithy.DeprecatedServiceGen
import playground.Assertions._
import playground.ServiceUtils._
import playground.language.Diffs.given
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.StringRangeUtils.*
import playground.smithyql.syntax.*
import playground.std.ClockGen
import playground.std.ClockOperation
import playground.std.RandomGen
import playground.std.RandomOperation
import weaver.*

object CompletionProviderTests extends SimpleIOSuite {

  pureTest("completing empty file - one service exists") {
    val random = wrapService(RandomGen)
    val provider = CompletionProvider.forServices(List(random))

    val result = provider.provide(
      "",
      Position(0),
    )

    val expected =
      RandomGen
        .endpoints
        .map { endpoint =>
          CompletionItem.forOperation(
            insertUseClause = CompletionItem.InsertUseClause.Required,
            endpoint,
            QualifiedIdentifier.forService(RandomGen),
            CompletionItem.InsertBodyStruct.Yes,
          )
        }
        .toList

    assertNoDiff(result, expected)
  }

  pureTest("completing empty file - multiple services exist") {

    val clock = wrapService(ClockGen)
    val random = wrapService(RandomGen)

    val provider = CompletionProvider.forServices(List(clock, random))

    val result = provider.provide(
      "",
      Position.origin,
    )

    val expected = List(
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.Required,
        endpoint = ClockOperation.CurrentTimestamp,
        serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
        CompletionItem.InsertBodyStruct.Yes,
      ),
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.Required,
        endpoint = RandomOperation.NextUUID,
        serviceId = QualifiedIdentifier.fromShapeId(RandomGen.id),
        CompletionItem.InsertBodyStruct.Yes,
      ),
    )

    assertNoDiff(result, expected)
  }

  pureTest("completing existing operation name doesn't insert struct") {
    val service = wrapService(RandomGen)

    val provider = CompletionProvider.forServices(List(service))

    val input = """playground.std#Random.NextUUID {}""".stripMargin
    val result = provider.provide(
      input,
      input.positionOf("NextUUID"),
    )

    val expected = List(
      CompletionItem.forOperation(
        CompletionItem.InsertUseClause.NotRequired,
        RandomOperation.NextUUID,
        QualifiedIdentifier.forService(service.service),
        CompletionItem.InsertBodyStruct.No,
      )
    )

    assertNoDiff(result, expected)
  }

  pureTest("completing existing use clause") {
    val service = wrapService(DemoServiceGen)

    val provider = CompletionProvider.forServices(List(service))

    val result = provider.provide(
      """use service a#B
        |hello {}""".stripMargin,
      Position("use service ".length),
    )

    val expected = List(
      CompletionItem.useServiceClause(
        QualifiedIdentifier.fromShapeId(DemoServiceGen.id),
        service,
      )
    )

    assert(result == expected)
  }

  pureTest(
    "the file has a use clause - completing operations shows results from that clause, but also others"
  ) {
    val provider = CompletionProvider.forServices(
      List(wrapService(ClockGen), wrapService(RandomGen))
    )
    val input =
      """use service playground.std#Clock
        |""".stripMargin.stripMargin

    val result = provider.provide(
      input,
      input.lastPosition,
    )

    val expected = List(
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.NotRequired,
        endpoint = ClockOperation.CurrentTimestamp,
        serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
        CompletionItem.InsertBodyStruct.Yes,
      ),
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.Required,
        endpoint = RandomOperation.NextUUID,
        serviceId = QualifiedIdentifier.fromShapeId(RandomGen.id),
        CompletionItem.InsertBodyStruct.Yes,
      ),
    )

    assertNoDiff(result, expected)
  }

  locally {
    pureTest("completing empty file - one (deprecated) service exists") {
      val provider = CompletionProvider.forServices(List(wrapService(DeprecatedServiceGen)))

      val result = provider
        .provide(
          "",
          Position(0),
        )
        .map(cit => (cit.deprecated, cit.kind))

      assert(result == List(true -> CompletionItemKind.Function))
    }

    pureTest("completing use clause - one (deprecated) service exists") {
      val provider = CompletionProvider.forServices(List(wrapService(DeprecatedServiceGen)))

      val result = provider
        .provide(
          "use service a#B\nhello {}",
          Position("use service ".length),
        )
        .map(cit => (cit.deprecated, cit.kind))

      assert(result == List(true -> CompletionItemKind.Module))
    }
  }

  pureTest("completing operation - multiple services available") {
    val clock = wrapService(ClockGen)
    val random = wrapService(RandomGen)
    val provider = CompletionProvider.forServices(List(clock, random))

    val input =
      """use service playground.std#Clock
        |hello {}
        |""".stripMargin

    val result = provider.provide(
      input,
      input.lastPosition,
    )

    val expected = List(
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.NotRequired,
        ClockOperation.CurrentTimestamp,
        QualifiedIdentifier.forService(ClockGen),
        CompletionItem.InsertBodyStruct.Yes,
      ),
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.Required,
        RandomOperation.NextUUID,
        QualifiedIdentifier.forService(RandomGen),
        CompletionItem.InsertBodyStruct.Yes,
      ),
    )

    assertNoDiff(result, expected)
  }

  // needs: completions inside prelude (entire use clauses)
  // needs: completions inside use clause (only service id)
  // https://github.com/kubukoz/smithy-playground/issues/163
}
