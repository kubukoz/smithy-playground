// package playground.smithyql.format

// import cats.Show
// import org.scalacheck.Arbitrary
// import playground.smithyql.SourceFile
// import playground.smithyql.WithSource
// import playground.smithyql.format.Formatter
// import playground.smithyql.parser.SourceParser
// import weaver._
// import weaver.scalacheck._

// object CommentFormattingTests extends SimpleIOSuite with Checkers {

//   implicit val showFile: Show[SourceFile[WithSource]] = Show.fromToString
//   implicit val arbFile: Arbitrary[SourceFile[WithSource]] = ???

//   test("Any query can be parsed back to the same query (minus comments)") {
//     forall { (q: SourceFile[WithSource]) =>
//       val formatted = Formatter[SourceFile].format(q, 80)

//       val unwrapQ = q.mapK(WithSource.unwrap)
//       SourceParser[SourceFile].parse(formatted) match {
//         case Left(e)  => failure(e.msg)
//         case Right(v) => assert(unwrapQ == v.mapK(WithSource.unwrap))
//       }
//     }
//   }
// }
