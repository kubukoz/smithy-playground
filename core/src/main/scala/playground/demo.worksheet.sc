import scala.util.control.NoStackTrace

import playground.AST

import playground.AST.Token._

import playground.SmithyQLParser
import cats.implicits._
val input = """
// before op
op
// after op
 {
  //inside struct
  firstKey = "firstValue"

 }
"""

val q =
  SmithyQLParser
    .parser
    .parseAll(input)
    .leftMap(e =>
      throw new Throwable(e.toString) with NoStackTrace {
        override def getMessage(): String = e.toString()
      }
    )
    .merge

q.operationName == AST.WithSource(
  value = "op",
  tokens = List(
    AST.Token.Comment(text = " before op"),
    Identifier(value = "op"),
    Comment(text = " after op"),
  ),
)

q.input
