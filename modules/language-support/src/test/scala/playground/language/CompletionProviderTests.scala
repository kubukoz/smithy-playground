package playground.language

import cats.Id
import cats.data.NonEmptyList
import cats.implicits._
import demo.smithy.DemoServiceGen
import demo.smithy.DeprecatedServiceGen
import playground.Assertions._
import playground.smithyql.OperationName
import playground.std.ClockGen
import playground.std.RandomGen
import smithy4s.Service
import smithy4s.dynamic.DynamicSchemaIndex
import weaver._

import playground.smithyql.syntax._
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Position
import playground.language.Diffs._

object CompletionProviderTests extends SimpleIOSuite {

  private def wrap[Algg[_[_, _, _, _, _]]](
    svc: Service[Algg]
  ): DynamicSchemaIndex.ServiceWrapper =
    new DynamicSchemaIndex.ServiceWrapper {
      type Alg[Oppp[_, _, _, _, _]] = Algg[Oppp]

      val service: Service[Alg] = svc
    }

  private val demoServiceId = QualifiedIdentifier.of("demo", "smithy", "DemoService")

  pureTest("completing existing use clause") {
    val service = wrap(DemoServiceGen)

    val provider = CompletionProvider.forServices(List(service))

    val result = provider.provide(
      """use service a#B
        |hello {}""".stripMargin,
      Position("use service ".length),
    )

    val expected = List(
      CompletionItem.useServiceClause(
        demoServiceId,
        service,
      )
    )

    assert(result == expected)
  }

  pureTest("completing empty file") {

    val service = wrap(DemoServiceGen)
    val random = wrap(RandomGen)

    val provider = CompletionProvider.forServices(List(service, random))

    val result = provider.provide(
      "",
      Position(0),
    )

    val expected =
      DemoServiceGen
        .endpoints
        .map(endpoint =>
          CompletionItem.forOperation(
            insertUseClause = CompletionItem
              .InsertUseClause
              .Required(
                List(
                  OperationName[Id]("CreateHero"),
                  OperationName[Id]("CreateSubscription"),
                  OperationName[Id]("GetPowers"),
                ).tupleRight(NonEmptyList.one(demoServiceId)).toMap
              ),
            endpoint,
            demoServiceId,
          )
        ) ++ RandomGen
        .endpoints
        .map(endpoint =>
          CompletionItem.forOperation(
            insertUseClause = CompletionItem
              .InsertUseClause
              .Required(
                Map(
                  OperationName[Id]("NextUUID") -> NonEmptyList
                    .one(QualifiedIdentifier.forService(RandomGen))
                )
              ),
            endpoint,
            QualifiedIdentifier.forService(RandomGen),
          )
        )

    assertNoDiff(result, expected)
  }

  pureTest("completing empty file - one service exists") {
    val random = wrap(RandomGen)
    val provider = CompletionProvider.forServices(List(random))

    val result = provider.provide(
      "",
      Position(0),
    )

    val expected = RandomGen
      .endpoints
      .map(endpoint =>
        CompletionItem.forOperation(
          insertUseClause =
            CompletionItem
              .InsertUseClause
              .NotRequired,
          endpoint,
          QualifiedIdentifier.forService(RandomGen),
        )
      )

    assert(result == expected)
  }

  locally {
    // for some reason, this can't be defined within the test body.
    val provider = CompletionProvider.forServices(List(wrap(DeprecatedServiceGen)))

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
    val clock = wrap(ClockGen)
    val random = wrap(RandomGen)
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
