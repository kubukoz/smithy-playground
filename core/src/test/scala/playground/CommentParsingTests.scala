package playground

import munit.FunSuite

class CommentParsingTests extends FunSuite {
  test("Comments from entire query are retained while parsing") {
    assertEquals(
      SmithyQLParser.parser.parseAll(Examples.fullOfComments).map(AST.WithSource.allQueryComments),
      Right(
        List(
          AST.Comment(" before op"),
          AST.Comment(" after op"),
          AST.Comment("before key"),
          AST.Comment(" after key"),
          AST.Comment("  before value"),
          AST.Comment("  after value"),
          AST.Comment(" before another key"),
          AST.Comment(" after second key"),
          AST.Comment(" before value"),
          AST.Comment(" after value"),
          AST.Comment("after trailing comma, technically this is part of the struct"),
          AST.Comment("  after whole thing"),
        )
      ),
    )
  }
}
