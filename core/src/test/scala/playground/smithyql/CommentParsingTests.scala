package playground.smithyql

import cats.Show
import weaver._
import weaver.scalacheck.Checkers

import Arbitraries._

object CommentParsingTests extends SimpleIOSuite with Checkers {
  pureTest("Comments from entire query are retained while parsing") {
    assert.eql(
      SmithyQLParser.parseFull(Examples.fullOfComments).map(WithSource.allQueryComments),
      Right(
        List(
          Comment(" before op"),
          Comment(" after op"),
          Comment("before key"),
          Comment(" after key"),
          Comment("  before value"),
          Comment("  after value"),
          Comment(" before another key"),
          Comment(" after second key"),
          Comment(" before value"),
          Comment(" after value"),
          Comment("after trailing comma, technically this is part of the struct"),
          Comment("  after whole thing"),
        )
      ),
    )
  }

  implicit val showQuery: Show[Query[WithSource]] = Show.fromToString
  implicit val showStruct: Show[Struct[WithSource]] = Show.fromToString

  test("Any query can be parsed back") {
    forall { (q: Query[WithSource]) =>
      val formatted = playground.smithyql.Formatter.format(q, 80)

      SmithyQLParser.parseFull(formatted) match {
        case Left(e) => failure(e.msg)
        case _       => success
      }
    }
  }
}
