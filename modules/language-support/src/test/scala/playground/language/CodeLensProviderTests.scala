package playground.language

import cats.effect.IO
import cats.syntax.all.*
import playground.Assertions.*
import playground.CompilationError
import playground.CompilationFailed
import playground.FileCompiler
import playground.FileRunner
import playground.OperationCompiler
import playground.OperationRunner
import playground.PreludeCompiler
import playground.ServiceIndex
import playground.ServiceUtils._
import playground.language.Diffs.given
import playground.smithyql.StringRangeUtils._
import playground.std.RandomGen
import weaver.*

object CodeLensProviderTests extends FunSuite {

  private val runnerStub = FileRunner.instance[IO](
    (
      _,
      _,
    ) =>
      ((
        _ => IO.stub
      ): OperationRunner[IO]).rightIor
  )

  private val services = List(wrapService(RandomGen))

  private val provider = CodeLensProvider.instance(
    FileCompiler
      .instance(
        PreludeCompiler.instance[CompilationError.InIorNel](ServiceIndex.fromServices(services)),
        OperationCompiler.fromServices(services),
      )
      // this shouldn't be here (it's a responsibility of file compiler)
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

  test("file is runnable, but it's an output panel") {
    val input = "playground.std#Random.NextUUID {}"
    assertNoDiff(
      provider.provide(Uri.fromUriString("output:anything"), input),
      Nil,
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
