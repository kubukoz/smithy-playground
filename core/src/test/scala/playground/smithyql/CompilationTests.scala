package playground.smithyql

import cats.Show
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.implicits._
import demo.smithy.Bad
import demo.smithy.FriendSet
import demo.smithy.Good
import demo.smithy.Hero
import demo.smithy.IntSet
import demo.smithy.Ints
import demo.smithy.Power
import org.scalacheck.Arbitrary
import playground.CompilationError
import playground.CompilationErrorDetails
import playground.PartialCompiler
import playground.QueryCompiler
import smithy.api.TimestampFormat
import smithy4s.Document
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.dynamic.model.IdRef
import smithy4s.dynamic.model.MemberShape
import smithy4s.dynamic.model.Model
import smithy4s.dynamic.model.Shape
import smithy4s.dynamic.model.StructureShape
import smithy4s.schema.Schema
import weaver._
import weaver.scalacheck.Checkers

import java.util.UUID

import Arbitraries._

object CompilationTests extends SimpleIOSuite with Checkers {

  import DSL._

  def compile[A: smithy4s.Schema](
    in: PartialCompiler.WAST
  ) = implicitly[smithy4s.Schema[A]].compile(QueryCompiler).compile(in)

  val dynamicPersonSchema = {
    val model = Model(
      smithy = Some("1.0"),
      shapes = Map(
        IdRef("test#Person") ->
          Shape.StructureCase(
            StructureShape(
              members = Some(
                Map(
                  "name" -> MemberShape(
                    target = IdRef("smithy.api#String"),
                    traits = Some(Map(IdRef("smithy.api#required") -> Document.obj())),
                  ),
                  "age" -> MemberShape(
                    target = IdRef("smithy.api#Integer")
                  ),
                )
              )
            )
          )
      ),
    )

    DynamicSchemaIndex
      .load(model)
      .getSchema(ShapeId("test", "Person"))
      .get
  }

  val dynamicPersonToDocument = Document
    .Encoder
    .fromSchema(dynamicPersonSchema)
    .asInstanceOf[Document.Encoder[Any]]

  pureTest("string") {
    assert(
      compile {
        WithSource.liftId("foo".mapK(WithSource.liftId))
      }(Schema.string) == Ior.right("foo")
    )
  }

  pureTest("string - got int instead") {
    assert(
      compile {
        WithSource.liftId(42.mapK(WithSource.liftId))
      }(Schema.string) == Ior.left(
        NonEmptyChain.of(
          CompilationError(
            CompilationErrorDetails.TypeMismatch(
              NodeKind.StringLiteral,
              NodeKind.IntLiteral,
            ),
            SourceRange(Position(0), Position(0)),
          )
        )
      )
    )
  }

  pureTest("int") {
    assert(
      compile {
        WithSource.liftId(42.mapK(WithSource.liftId))
      }(Schema.int) == Ior.right(42)
    )
  }

  pureTest("boolean") {
    assert(
      compile {
        WithSource.liftId(true.mapK(WithSource.liftId))
      }(Schema.boolean) == Ior.right(true)
    )
  }

  pureTest("Simple struct") {
    assert(
      compile[Good] {
        WithSource.liftId {
          struct("howGood" -> 200).mapK(WithSource.liftId)
        }
      } == Ior.right(Good(200))
    )
  }

  pureTest("Missing fields in struct") {
    assert(
      compile[Bad] {
        WithSource.liftId {
          struct().mapK(WithSource.liftId)
        }
      } == Ior.left(
        NonEmptyChain.of(
          CompilationError(
            CompilationErrorDetails
              .MissingField("evilName"),
            SourceRange(Position(0), Position(0)),
          ),
          CompilationError(
            CompilationErrorDetails.MissingField("powerLevel"),
            SourceRange(Position(0), Position(0)),
          ),
        )
      )
    )
  }

  pureTest("Missing fields in struct - 1 already present") {
    assert(
      compile[Bad] {
        WithSource.liftId {
          struct("evilName" -> "hello").mapK(WithSource.liftId)
        }
      } == Ior.left(
        NonEmptyChain.of(
          CompilationError(
            CompilationErrorDetails.MissingField("powerLevel"),
            SourceRange(Position(0), Position(0)),
          )
        )
      )
    )
  }
  pureTest("union") {
    assert(
      compile[Hero] {
        WithSource.liftId {
          struct(
            "good" -> struct("howGood" -> 200)
          ).mapK(WithSource.liftId)
        }
      } == Ior.right(Hero.GoodCase(Good(200)))
    )
  }

  pureTest("timestamp - OK") {
    val result =
      compile(WithSource.liftId("2022-07-11T17:42:28.000Z".mapK(WithSource.liftId)))(
        Schema.timestamp
      )
    val expected = Timestamp.parse("2022-07-11T17:42:28.000Z", TimestampFormat.DATE_TIME).get

    assert(
      result == Ior.right(expected)
    )
  }

  pureTest("timestamp - ignores format") {
    val result =
      compile(WithSource.liftId("2022-07-11T17:42:28.000Z".mapK(WithSource.liftId)))(
        Schema.timestamp.addHints(TimestampFormat.EPOCH_SECONDS: TimestampFormat)
      )

    val expected = Timestamp.parse("2022-07-11T17:42:28.000Z", TimestampFormat.DATE_TIME).get

    assert(
      result == Ior.right(expected)
    )
  }

  pureTest("timestamp - fails when the format is invalid") {
    val result = compile(WithSource.liftId("not-a-timestamp".mapK(WithSource.liftId)))(
      Schema.timestamp
    ).leftMap(_.map(_.err))

    // todo: timestamp parsing always passes on JS but does something weird.
    // this is fixed in smithy4s 0.14.0
    assert(
      result == Ior.leftNec(
        CompilationErrorDetails.InvalidTimestampFormat(TimestampFormat.DATE_TIME)
      )
    )
  }

  pureTest("uuid - OK") {
    val result =
      compile(
        WithSource.liftId("9c8f8f8f-8f8f-8f8f-8f8f-8f8f8f8f8f8f".mapK(WithSource.liftId))
      )(
        Schema.uuid
      )
    assert(
      result == Ior.right(UUID.fromString("9c8f8f8f-8f8f-8f8f-8f8f-8f8f8f8f8f8f"))
    )
  }

  pureTest("enum - OK") {
    assert(
      compile[Power](WithSource.liftId("Wind".mapK(WithSource.liftId))) == Ior.right(Power.WIND)
    )
  }

  pureTest("enum - failure") {
    assert(
      compile[Power](WithSource.liftId("Poison".mapK(WithSource.liftId))) == Ior.left(
        NonEmptyChain.of(
          CompilationError(
            CompilationErrorDetails.UnknownEnumValue(
              "Poison",
              List("Ice", "Fire", "Lightning", "Wind"),
            ),
            SourceRange(Position(0), Position(0)),
          )
        )
      )
    )
  }

  pureTest("list of ints") {
    assert(
      compile[Ints](WithSource.liftId(List(1, 2, 3).mapK(WithSource.liftId))) == Ior.right(
        Ints(List(1, 2, 3))
      )
    )
  }

  pureTest("set of ints") {
    assert(
      compile[IntSet](WithSource.liftId(List(1, 2, 3).mapK(WithSource.liftId))) == Ior.right(
        IntSet(Set(1, 2, 3))
      )
    )
  }

  test("set of ints has warnings when duplicates are found") {
    forall { (range1: SourceRange, range2: SourceRange, range3: SourceRange) =>
      val actual = compile[IntSet](
        WithSource.liftId(
          Listed[WithSource](
            WithSource.liftId(
              List(
                WithSource.liftId(IntLiteral[WithSource](1)).withRange(range1),
                WithSource.liftId(IntLiteral[WithSource](2)).withRange(range2),
                WithSource.liftId(IntLiteral[WithSource](2)).withRange(range3),
              )
            )
          )
        )
      )

      assert(
        actual == Ior.both(
          NonEmptyChain(
            CompilationError(
              CompilationErrorDetails.DuplicateItem,
              range2,
            ),
            CompilationError(
              CompilationErrorDetails.DuplicateItem,
              range3,
            ),
          ),
          Set(1, 2),
        )
      )
    }
  }

  pureTest("set of struct fails when duplicates are found") {

    val item = struct("good" -> struct("howGood" -> 42))
    val compiledFailures = compile[FriendSet](
      WithSource.liftId(
        List(
          item,
          item,
        ).mapK(WithSource.liftId)
      )
    )
      .leftMap(_.map(_.err))

    assert(
      compiledFailures == Ior.both(
        NonEmptyChain(CompilationErrorDetails.DuplicateItem, CompilationErrorDetails.DuplicateItem),
        Set(Hero.GoodCase(Good(42))),
      )
    )
  }

  pureTest("set of struct has warnings when duplicates are found - dynamic") {
    val compiledFailures = compile(
      WithSource.liftId(
        List(
          struct("name" -> "Hello"),
          struct("name" -> "Hello"),
        ).mapK(WithSource.liftId)
      )
    )(Schema.set(dynamicPersonSchema))
      .map(_.map(dynamicPersonToDocument.encode(_)))
      .leftMap(_.map(_.err))

    val expected = Set(
      Document.obj(
        "name" -> Document.fromString("Hello")
      )
    )

    assert(
      compiledFailures == Ior.both(
        NonEmptyChain(CompilationErrorDetails.DuplicateItem, CompilationErrorDetails.DuplicateItem),
        expected,
      )
    )
  }

  pureTest("set of struct is OK when optional fields differ - dynamic") {
    val compiledCount = compile(
      WithSource.liftId(
        List(
          struct("name" -> "Hello", "age" -> 42),
          struct("name" -> "Hello"),
        ).mapK(WithSource.liftId)
      )
    )(Schema.set(dynamicPersonSchema)).map(_.map(dynamicPersonToDocument.encode(_)))

    val expected = Set(
      Document.obj(
        "name" -> Document.fromString("Hello"),
        "age" -> Document.fromInt(42),
      ),
      Document.obj(
        "name" -> Document.fromString("Hello")
      ),
    )

    assert(
      compiledCount == Ior.right(
        expected
      )
    )
  }

  pureTest("list of strings where a list of ints is expected") {
    assert(
      compile[Ints](WithSource.liftId(List("hello", "world").mapK(WithSource.liftId))) == Ior.left(
        NonEmptyChain.of(
          CompilationError(
            CompilationErrorDetails.TypeMismatch(NodeKind.IntLiteral, NodeKind.StringLiteral),
            SourceRange(Position(0), Position(0)),
          ),
          CompilationError(
            CompilationErrorDetails.TypeMismatch(NodeKind.IntLiteral, NodeKind.StringLiteral),
            SourceRange(Position(0), Position(0)),
          ),
        )
      )
    )
  }

  implicit val arbInputNode = Arbitrary(genInputNode(2))
  implicit val showWast: Show[PartialCompiler.WAST] = Show.fromToString

  test("anything to document matches") {
    forall((wast: PartialCompiler.WAST) =>
      assert(
        compile[Document](wast)(Schema.document).isRight
      )
    )
  }

  pureTest("list of structs to document") {
    assert(
      compile(
        WithSource.liftId(
          List(
            struct("good" -> true, "howGood" -> 200),
            struct("name" -> "aaa"),
          ).mapK(WithSource.liftId)
        )
      )(Schema.document) == Ior.right(
        Document.array(
          Document.obj(
            "good" -> Document.fromBoolean(true),
            "howGood" -> Document.fromInt(200),
          ),
          Document.obj(
            "name" -> Document.fromString("aaa")
          ),
        )
      )
    )
  }
}
