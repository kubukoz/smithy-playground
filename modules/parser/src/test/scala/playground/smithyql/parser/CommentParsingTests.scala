package playground.smithyql.parser

import playground.smithyql.Comment
import playground.smithyql.SourceFile
import playground.smithyql.WithSource
import weaver.*

object CommentParsingTests extends SimpleIOSuite {

  pureTest("Comments from entire query are retained while parsing") {
    assert.eql(
      SourceParser[SourceFile].parse(Examples.fullOfComments).map(WithSource.allSourceComments),
      Right(
        List(
          Comment(" before use clause"),
          Comment(" before another clause"),
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
