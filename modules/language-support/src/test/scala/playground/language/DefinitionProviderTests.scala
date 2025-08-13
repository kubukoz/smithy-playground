package playground.language

import cats.Id
import cats.effect.IO
import demo.smithy.DemoService
import demo.smithy.DemoServiceGen
import demo.smithy.DeprecatedServiceGen
import playground.Assertions.*
import playground.Location
import playground.ServiceIndex
import playground.ServiceIndex.OperationMetadata
import playground.ServiceIndex.ServiceMetadata
import playground.ServiceUtils.*
import playground.TestTextUtils
import playground.TestTextUtils.CURSOR
import playground.Uri
import playground.language.Diffs.given
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.SourceRange
import playground.smithyql.StringRangeUtils.*
import playground.smithyql.syntax.*
import playground.std.Clock
import playground.std.ClockGen
import playground.std.ClockOperation
import playground.std.RandomGen
import playground.std.RandomOperation
import weaver.*

object DefinitionProviderTests extends FunSuite {

  private def getDefinition(
    inputFileWithCursor: String
  ) = {

    val (remainingString, cursorPosition) = TestTextUtils.extractCursor(inputFileWithCursor)

    given TextDocumentProvider[Id] = TextDocumentProvider.always[Id](
      remainingString
    )

    val provider = DefinitionProvider.instance[Id](
      ServiceIndex.fromMappings(
        Map(
          QualifiedIdentifier.fromShapeId(Clock.id) -> ServiceMetadata(
            operations = Set(
              OperationMetadata(
                name = OperationName("CurrentTimestamp"),
                location = Some(
                  Location
                    .Empty
                    .copy(document = Uri.fromUriString("smithyjar://stdlib-operation.jar"))
                ),
              )
            ),
            deprecated = None,
            location = Some(
              Location.Empty.copy(document = Uri.fromUriString("smithyjar://stdlib.jar"))
            ),
          )
        )
      )
    )

    provider.definition(
      Uri.fromUriString("/"),
      cursorPosition,
    )

  }

  test("Go-to definition finds locations in use clauses") {
    val locs = getDefinition(s"""use service playground.std#Clock${CURSOR}""")

    expect.same(
      List(Location.Empty.copy(document = Uri.fromUriString("smithyjar://stdlib.jar"))),
      locs,
    )
  }

  test("Go-to definition finds locations in explicit service references") {
    val locs = getDefinition(s"""playground.std#Clock${CURSOR}.CurrentTimestamp {}""")

    expect.same(
      List(Location.Empty.copy(document = Uri.fromUriString("smithyjar://stdlib.jar"))),
      locs,
    )
  }

  test("Go-to definition ignores parse failures") {
    val locs = getDefinition("""definitely not valid""".stripMargin)

    expect.same(
      Nil,
      locs,
    )
  }

  test("Go-to definition resolves implicit services for operation calls") {
    val locs = getDefinition(
      s"""use service playground.std#Clock
         |
         |CurrentTimestamp${CURSOR} {}""".stripMargin
    )

    expect.same(
      List(Location.Empty.copy(document = Uri.fromUriString("smithyjar://stdlib-operation.jar"))),
      locs,
    )
  }

}
