package playground.smithyql

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object Arbitraries {

  implicit val arbitraryComment: Arbitrary[Comment] = Arbitrary {
    Gen
      .stringOf(
        Gen.asciiPrintableChar.filterNot(_ == '\n')
      )
      .map(s => " " + s.trim)
      .map(Comment(_))
  }

  implicit val arbOpName: Arbitrary[OperationName] = Arbitrary {
    Gen.identifier.map(OperationName(_))
  }

  implicit def arbitraryWithSource[A: Arbitrary]: Arbitrary[WithSource[A]] = Arbitrary {
    val comments = Gen.choose(0, 3).flatMap(Gen.listOfN(_, arbitraryComment.arbitrary))

    for {
      left <- comments
      right <- comments
      v <- Arbitrary.arbitrary[A]
    } yield WithSource[A](left, right, v)
  }

  implicit val arbIntLiteral: Arbitrary[IntLiteral[WithSource]] = Arbitrary {
    Gen.resultOf(IntLiteral.apply[WithSource])
  }

  implicit val arbStringLiteral: Arbitrary[StringLiteral[WithSource]] = Arbitrary {
    implicit val arbString = Arbitrary {
      Gen
        .stringOf(Gen.asciiPrintableChar.filterNot("\n\"".contains(_)))
    }

    Gen.resultOf(StringLiteral.apply[WithSource])
  }

  def genInputNode(depth: Int): Gen[InputNode[WithSource]] =
    if (depth > 0)
      Gen.oneOf(
        arbStringLiteral.arbitrary,
        arbIntLiteral.arbitrary,
        genStruct(depth),
      )
    else
      Gen.oneOf(
        arbStringLiteral.arbitrary,
        arbIntLiteral.arbitrary,
      )

  implicit val arbStructKey: Arbitrary[Struct.Key] = Arbitrary {
    Gen.identifier.map(Struct.Key(_))
  }

  def genStruct(depth: Int): Gen[Struct[WithSource]] = {
    implicit val arbNodes: Arbitrary[InputNode[WithSource]] = Arbitrary(genInputNode(depth - 1))

    Gen.resultOf(Struct.apply[WithSource])
  }

  implicit val arbStruct: Arbitrary[Struct[WithSource]] = Arbitrary(genStruct(2))

  implicit val arbQuery: Arbitrary[Query[WithSource]] = Arbitrary {
    Gen.resultOf(Query.apply[WithSource])
  }

}
