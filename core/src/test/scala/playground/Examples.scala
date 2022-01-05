package playground

object Examples {
  val fullOfComments = """
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
}
