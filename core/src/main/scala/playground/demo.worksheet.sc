import schematic.bytes

import playground.AST

import playground.AST.high._
import playground.AST.WithSource

import playground.SmithyQLParser
import cats.implicits._

val input = """
// before op
op
// after op
 {

  //before key
  firstKey
  // after key
   =
    //  before value
     "firstValue"
    //  after value
  ,
  // before another key
  secondKey
  // after second key
  =
    // before value
    "secondValue"
    // after value
    ,
    //after trailing comma, technically this is part of the struct
 }
//  after whole thing
"""

val qRaw = SmithyQLParser.parser.parseAll(input).toOption.get
val q = SmithyQLParser.parse(input).toTry.get

def allCommentsOf(input: WithSource[Query[WithSource]]) = {

  def comments(node: InputNode[AST.WithSource]): List[AST.Comment] = node.fold(
    struct = _.fields.allComments(_.flatMap { case (k, v) =>
      k.allComments(_ => Nil) ++ v.fold(comments, comments, comments)
    }.toList),
    string = _.value.allComments(_ => Nil),
    int = _.value.allComments(_ => Nil),
  )

  input.allComments { q =>
    q.operationName.allComments(_ => Nil) ++
      q.input.fold(comments, comments, comments)
  }
}

allCommentsOf(qRaw) == (
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
  ),
)
