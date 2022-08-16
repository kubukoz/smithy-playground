package playground.smithyql

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.data.NonEmptyList

object Arbitraries {
  implicit val arbitraryString: Arbitrary[String] = Arbitrary(Gen.asciiPrintableStr)

  implicit def arbitraryNel[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Arbitrary(
    Gen.resultOf(NonEmptyList.apply[A])
  )

  implicit val arbitraryComment: Arbitrary[Comment] = Arbitrary {
    Gen
      .stringOf(
        Gen.asciiPrintableChar.filterNot(_ == '\n')
      )
      .map(Comment(_))
  }

  implicit val arbOpName: Arbitrary[OperationName[WithSource]] = Arbitrary {
    Gen.identifier.map(OperationName(_))
  }

  implicit val arbitraryPosition: Arbitrary[Position] = Arbitrary(Gen.resultOf(Position.apply))

  // todo: valid ranges
  implicit val arbitraryRange: Arbitrary[SourceRange] = Arbitrary(Gen.resultOf(SourceRange.apply))

  implicit def arbitraryWithSource[A: Arbitrary]: Arbitrary[WithSource[A]] = Arbitrary {
    val comments = Gen.choose(0, 3).flatMap(Gen.listOfN(_, arbitraryComment.arbitrary))

    for {
      left <- comments
      right <- comments
      range <- arbitraryRange.arbitrary
      v <- Arbitrary.arbitrary[A]
      // todo: positions not checked in these tests
    } yield WithSource[A](left, right, range = range, v)
  }

  implicit val arbIntLiteral: Arbitrary[IntLiteral[WithSource]] = Arbitrary {
    Arbitrary.arbBigDecimal.arbitrary.map { bd =>
      IntLiteral[WithSource](bd.toString())
    }
  }

  implicit val arbStringLiteral: Arbitrary[StringLiteral[WithSource]] = Arbitrary {
    Gen
      .stringOf(Gen.asciiPrintableChar.filterNot("\n\"".contains(_)))
      .map(StringLiteral(_))
  }

  implicit val arbQualifiedIdentifier: Arbitrary[QualifiedIdentifier] = Arbitrary(
    Gen.resultOf(QualifiedIdentifier.apply)
  )

  implicit val arbitraryUseClause: Arbitrary[UseClause[WithSource]] = Arbitrary(
    Gen.resultOf(UseClause.apply[WithSource])
  )

  implicit val arbBool: Arbitrary[BooleanLiteral[WithSource]] = Arbitrary {
    Gen.resultOf(BooleanLiteral[WithSource])
  }

  def genInputNode(depth: Int): Gen[InputNode[WithSource]] =
    if (depth > 0) {
      val deeper = genInputNode(depth - 1)
      Gen.oneOf(
        arbStringLiteral.arbitrary,
        arbIntLiteral.arbitrary,
        genStruct(deeper),
        genSequence(deeper).arbitrary,
      )
    } else
      Gen.oneOf(
        arbStringLiteral.arbitrary,
        arbIntLiteral.arbitrary,
        arbBool.arbitrary,
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

  def genSequence(
    recurse: Gen[InputNode[WithSource]]
  ): Arbitrary[Listed[WithSource]] = {
    implicit val arbNodes: Arbitrary[InputNode[WithSource]] = Arbitrary(recurse)

    Arbitrary(Gen.resultOf(Listed[WithSource]))
  }

  implicit val arbStruct: Arbitrary[Struct[WithSource]] = Arbitrary(genStruct(genInputNode(1)))

  implicit val arbQuery: Arbitrary[Query[WithSource]] = Arbitrary {
    Gen.resultOf(Query.apply[WithSource])
  }

}
