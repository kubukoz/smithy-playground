package playground.smithyql

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object Arbitraries {

  implicit val arbitraryComment: Arbitrary[Comment] = Arbitrary {
    Gen
      .stringOf(
        Gen.asciiPrintableChar.filterNot(_ == '\n')
      )
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
      // todo: positions not checked in these tests
    } yield WithSource[A](left, right, range = SourceRange(Position(0), Position(0)), v)
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

  // todo: support all kinds of input nodes
  def genInputNode(depth: Int): Gen[InputNode[WithSource]] =
    if (depth > 0)
      Gen.oneOf(
        arbStringLiteral.arbitrary,
        arbIntLiteral.arbitrary,
        genStruct(genInputNode(depth - 1)),
      )
    else
      Gen.oneOf(
        arbStringLiteral.arbitrary,
        arbIntLiteral.arbitrary,
      )

  implicit val arbStructKey: Arbitrary[Struct.Key] = Arbitrary {
    Gen.identifier.map(Struct.Key(_))
  }

  implicit def arbFields[F[_]](
    implicit arbKey: Arbitrary[F[Struct.Key]],
    arbValue: Arbitrary[F[InputNode[F]]],
  ): Arbitrary[Struct.Fields[F]] = Arbitrary(Gen.resultOf(Struct.Fields[F](_)))

  def genStruct(recurse: Gen[InputNode[WithSource]]): Gen[Struct[WithSource]] = {
    implicit val arbNodes: Arbitrary[InputNode[WithSource]] = Arbitrary(recurse)

    Gen
      .resultOf(Struct.apply[WithSource])
  }

  implicit val arbStruct: Arbitrary[Struct[WithSource]] = Arbitrary(genStruct(genInputNode(1)))

  implicit val arbQuery: Arbitrary[Query[WithSource]] = Arbitrary {
    Gen.resultOf(Query.apply[WithSource])
  }

}
