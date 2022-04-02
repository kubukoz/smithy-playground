package playground.smithyql

import cats.implicits._
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

    Gen
      .resultOf(Struct.apply[WithSource])
      .map { struct =>
        Struct(
          // Workaround for no-mapiness of comments (two identical keys can have different sets of comments, and that makes the map non-unique)
          // TODO: Make this a parsing failure
          struct.fields.map(_.toList.distinctBy(_._1.value).toMap)
        )
      }
  }

  implicit val arbStruct: Arbitrary[Struct[WithSource]] = Arbitrary(genStruct(2))

  implicit val arbQuery: Arbitrary[Query[WithSource]] = Arbitrary {
    Gen.resultOf(Query.apply[WithSource])
  }

}
