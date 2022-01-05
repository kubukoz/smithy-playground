import scala.util.control.NoStackTrace

import playground.AST

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

qRaw
