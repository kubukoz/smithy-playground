package playground.smithyql

import weaver._

object CommentParsingTests extends FunSuite {
  test("Comments from entire query are retained while parsing") {
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
}
