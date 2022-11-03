package playground.language

import demo.smithy.DemoServiceGen
import demo.smithy.DeprecatedServiceGen
import playground.Assertions._
import playground.language.Diffs._
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.syntax._
import playground.std.ClockGen
import playground.std.RandomGen
import weaver._

import StringRangeUtils._
import ServiceUtils.wrapService

object CompletionProviderTests extends SimpleIOSuite {

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
        endpoint = ClockGen.CurrentTimestamp,
        serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
      ),
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.Required,
        endpoint = RandomGen.NextUUID,
        serviceId = QualifiedIdentifier.fromShapeId(RandomGen.id),
      ),
    )

    assertNoDiff(result, expected)
  }

  pureTest("completing empty file") {

    val clock = wrapService(ClockGen)
    val random = wrapService(RandomGen)

    val provider = CompletionProvider.forServices(List(clock, random))

    val result = provider.provide(
      "",
      Position(0),
    )

    val expected = List(
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.Required,
        endpoint = ClockGen.CurrentTimestamp,
        serviceId = QualifiedIdentifier.fromShapeId(ClockGen.id),
      ),
      CompletionItem.forOperation(
        insertUseClause = CompletionItem.InsertUseClause.Required,
        endpoint = RandomGen.NextUUID,
        serviceId = QualifiedIdentifier.fromShapeId(RandomGen.id),
      ),
    )

    assertNoDiff(result, expected)
  }

  pureTest("completing empty file - one service exists") {
    val random = wrapService(RandomGen)
    val provider = CompletionProvider.forServices(List(random))

    val result = provider.provide(
      "",
      Position(0),
    )

    val expected = RandomGen
      .endpoints
      .map(endpoint =>
        CompletionItem.forOperation(
          insertUseClause = CompletionItem.InsertUseClause.Required,
          endpoint,
          QualifiedIdentifier.forService(RandomGen),
        )
      )

    assertNoDiff(result, expected)
  }

  locally {
    // for some reason, this can't be defined within the test body.
    // https://github.com/disneystreaming/smithy4s/issues/537
    val provider = CompletionProvider.forServices(List(wrapService(DeprecatedServiceGen)))

    pureTest("completing empty file - one (deprecated) service exists") {
      val result = provider
        .provide(
          "",
          Position(0),
        )
        .map(cit => (cit.deprecated, cit.kind))

      assert(result == List(true -> CompletionItemKind.Function))
    }

    pureTest("completing use clause - one (deprecated) service exists") {
      val result = provider
        .provide(
          "use service a#B\nhello {}",
          Position("use service ".length),
        )
        .map(cit => (cit.deprecated, cit.kind))

      assert(result == List(true -> CompletionItemKind.Module))
    }
  }

  pureTest("completing operation - use clause exists, multiple services available") {
    val clock = wrapService(ClockGen)
    val random = wrapService(RandomGen)
    val provider = CompletionProvider.forServices(List(clock, random))

    val result = provider.provide(
      """use service playground.std#Clock
        |hello {}""".stripMargin,
      Position("use service playground.std#Clock\n".length),
    )

    val expected = ClockGen
      .service
      .endpoints
      .map(endpoint =>
        CompletionItem.forOperation(
          insertUseClause = CompletionItem.InsertUseClause.NotRequired,
          endpoint,
          QualifiedIdentifier.forService(ClockGen),
        )
      )

    assertNoDiff(result, expected)
  }

}
