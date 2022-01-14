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
      }(schematic.string.Schema) == Ior.right("foo")
    )
  }

  test("string - got int instead") {
    assert(
      compile {
        WithSource.liftId(42.mapK(WithSource.liftId))
      }(schematic.string.Schema) == Ior.left(
        NonEmptyChain.of(
          CompilationError(
            "Expected NodeKind.StringLiteral, got NodeKind.IntLiteral instead",
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
      }(schematic.int.Schema) == Ior.right(42)
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
          CompilationError("Missing field evilName", range = SourceRange(Position(0), Position(0))),
          CompilationError(
            "Missing field powerLevel",
            range = SourceRange(Position(0), Position(0)),
          ),
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
      compile[Power](WithSource.liftId("Posion".mapK(WithSource.liftId))) == Ior.left(
        NonEmptyChain.of(
          CompilationError(
            "Unknown enum value: Posion. Available values: Ice, Fire, Lightning, Wind",
            range = SourceRange(Position(0), Position(0)),
          )
        )
      )
    )
  }
}
