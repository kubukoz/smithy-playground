package playground.smithyql

import weaver._
import cats.implicits._
import cats.effect.IO
import playground.smithyql.parser.SmithyQLParser
import playground.Diffs._
import playground.Assertions._

object RangeIndexTests extends SimpleIOSuite {

  private def rangeIndex(q: String) = SmithyQLParser.parseFull(q).liftTo[IO].map(RangeIndex.build)

  test("simple program range index") { (_, l) =>
    implicit val log = l

    val q = """hello { isTest = true }"""
    rangeIndex(q).flatMap { i =>
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
