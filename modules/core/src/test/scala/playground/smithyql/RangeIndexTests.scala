package playground.smithyql

import weaver._
import cats.implicits._
import cats.effect.IO
import playground.Diffs._
import playground.Assertions._
import playground.smithyql.parser.SourceParser

object RangeIndexTests extends SimpleIOSuite {

  private def rangeIndex(q: String) = SourceParser[Query].parse(q).liftTo[IO].map(RangeIndex.build)

  test("simple program range index") {

    val q = """hello { isTest = true }"""
    rangeIndex(q).map { i =>
      val result = i.findAtPosition(Position("hello { is".length))

      assertNoDiff(
        result,
        Some(
          ContextRange(
            range = SourceRange(Position("hello {".length), Position(q.length - 1)),
            ctx = NodeContext.Root.inOperationInput.inStructBody,
          )
        ),
      )
    }

  }
}
