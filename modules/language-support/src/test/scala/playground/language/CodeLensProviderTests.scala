package playground.language

import cats.effect.IO
import cats.implicits._
import playground.Assertions._
import playground.CompilationFailed
import playground.FileCompiler
import playground.FileRunner
import playground.OperationCompiler
import playground.OperationRunner
import playground.language.Diffs._
import playground.std.RandomGen
import weaver._

import StringRangeUtils._

object CodeLensProviderTests extends FunSuite {

  private val runnerStub = FileRunner.instance[IO](_ =>
    ((_ => IO.stub): OperationRunner[IO]).rightIor
  )

  private val provider = CodeLensProvider.instance(
    FileCompiler
      .instance(OperationCompiler.fromService(RandomGen))
      .mapK(CompilationFailed.wrapK),
    runnerStub,
  )

  private val anyUri = Uri.fromUriString("file://test.smithyql")
  test("file parses with no queries - no lenses") {
    assertNoDiff(provider.provide(anyUri, ""), Nil)
  }

  test("file doesn't parse - no lenses") {
    assertNoDiff(provider.provide(anyUri, "$*&$(!&(*#@&"), Nil)
  }

  test("file parses - unknown/ambiguous service") {
    assertNoDiff(provider.provide(anyUri, "hello {}"), Nil)
  }

  test("file is runnable") {
    val input = "playground.std#Random.NextUUID {}"
    assertNoDiff(
      provider.provide(anyUri, input),
      List(
        CodeLens(
          range = input.rangeOf("playground.std#Random.NextUUID"),
          command = Command(
            title = "Run SmithyQL file",
            command = Command.RUN_FILE,
            args = List("file://test.smithyql"),
          ),
        )
      ),
    )
  }

  test("file is runnable - multiple queries") {
    val input =
      s"""playground.std#Random.NextUUID {}
         |playground.std#Random.NextUUID {}""".stripMargin

    assertNoDiff(
      provider.provide(anyUri, input),
      List(
        CodeLens(
          range = input.rangeOf("playground.std#Random.NextUUID"),
          command = Command(
            title = "Run SmithyQL file",
            command = Command.RUN_FILE,
            args = List("file://test.smithyql"),
          ),
        )
      ),
    )
  }
}
