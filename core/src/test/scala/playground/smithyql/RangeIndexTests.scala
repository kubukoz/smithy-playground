package playground.smithyql

import weaver._
import cats.implicits._
import cats.effect.IO

object RangeIndexTests extends SimpleIOSuite {

  private def rangeIndex(q: String) = SmithyQLParser.parseFull(q).liftTo[IO].map(RangeIndex.build)

  test("simple program range index") {
    val q = """hello { isTest = true }"""
    rangeIndex(
      q
    ).map { i =>
      val result = i.findAtPosition(Position("hello { is".length))

      assert(
        result == Some(
          ContextRange(
            range = SourceRange(Position("hello {".length), Position(q.length - 1)),
            ctx = NodeContext.InputContext.root.append(NodeContext.PathEntry.StructBody),
          )
        )
      )
    }

  }
}
