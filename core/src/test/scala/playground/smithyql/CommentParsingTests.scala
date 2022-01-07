package playground.smithyql

import cats.Show
import cats.implicits._
import weaver._
import weaver.scalacheck.Checkers

import Arbitraries._

object CommentParsingTests extends SimpleIOSuite with Checkers {
  import Assertions._
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

  pureTest("Comments aren't lost when formatting") {
    val result = SmithyQLParser
      .parseFull(Examples.fullOfComments)
      .map(playground.smithyql.Formatter.format(_, 80))
      .flatMap(SmithyQLParser.parseFull)

    assert.eql(
      result.map(WithSource.allQueryComments),
      Right(
        List(
          Comment(" before op"),
          Comment(" after op"),
          Comment(" before key"),
          Comment(" after key"),
          Comment("  before value"),
          Comment("  after value"),
          Comment(" before another key"),
          Comment(" after second key"),
          Comment(" before value"),
          Comment(" after value"),
          Comment(" after trailing comma, technically this is part of the struct"),
          Comment("  after whole thing"),
        )
      ),
    )
  }

  implicit val showQuery: Show[Query[WithSource]] = Show.fromToString
  implicit val showStruct: Show[Struct[WithSource]] = Show.fromToString

  test("Any query can be parsed back to the same query (minus comments)") {
    forall { (q: Query[WithSource]) =>
      val formatted = playground.smithyql.Formatter.format(q, 80)

      val unwrapQ = q.mapK(WithSource.unwrap)
      SmithyQLParser.parseFull(formatted) match {
        case Left(e)  => failure(e.msg)
        case Right(v) => compareQuery(unwrapQ, v.mapK(WithSource.unwrap))

      }
    }
  }

}
