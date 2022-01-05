import playground.smithyql.Formatter

import playground.smithyql.SmithyQLParser

val in = """// before op
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

val q = SmithyQLParser.parseFull(in).toTry.get

val pretty = Formatter.format(q, 40)

val reparsed = SmithyQLParser.parseFull(pretty).toTry.get

q.toString()
reparsed.toString()

Formatter.format(reparsed, 40)

require(q == reparsed)
