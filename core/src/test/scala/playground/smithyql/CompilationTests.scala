package playground.smithyql

import cats.data.Ior
import cats.data.NonEmptyChain
import demo.smithy.Bad
import demo.smithy.Good
import demo.smithy.Hero
import demo.smithy.Power
import playground.CompilationError
import playground.PartialCompiler
import playground.QueryCompilerSchematic
import weaver._
import smithy4s.schema.Schema
import playground.CompilationErrorDetails
import demo.smithy.Ints

object CompilationTests extends FunSuite {

  import DSL._

  val comp = new QueryCompilerSchematic

  def compile[A: smithy4s.Schema](
    in: PartialCompiler.WAST
  ) = implicitly[smithy4s.Schema[A]].compile(comp).compile(in)

  test("string") {
    assert(
      compile {
        WithSource.liftId("foo".mapK(WithSource.liftId))
      }(Schema.string) == Ior.right("foo")
    )
  }

  test("string - got int instead") {
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

  test("int") {
    assert(
      compile {
        WithSource.liftId(42.mapK(WithSource.liftId))
      }(Schema.int) == Ior.right(42)
    )
  }

  test("Simple struct") {
    assert(
      compile[Good] {
        WithSource.liftId {
          struct("howGood" -> 200).mapK(WithSource.liftId)
        }
      } == Ior.right(Good(200))
    )
  }

  test("Missing fields in struct") {
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

  test("Missing fields in struct - 1 already present") {
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
  test("union") {
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

  test("enum - OK") {
    assert(
      compile[Power](WithSource.liftId("Wind".mapK(WithSource.liftId))) == Ior.right(Power.WIND)
    )
  }

  test("enum - failure") {
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

  test("list of ints") {
    assert(
      compile[Ints](WithSource.liftId(List(1, 2, 3).mapK(WithSource.liftId))) == Ior.right(
        Ints(List(1, 2, 3))
      )
    )
  }

  test("list of strings where a list of ints is expected") {
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
}
