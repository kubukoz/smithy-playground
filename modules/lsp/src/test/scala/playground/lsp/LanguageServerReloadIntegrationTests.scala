package playground.lsp

import cats.effect.IO
import cats.implicits._
import fs2.io.file.Files
import fs2.io.file.Path
import org.eclipse.lsp4j.CodeLensParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import playground.BuildConfig
import playground.BuildConfigDecoder
import playground.language.Uri
import weaver._

object LanguageServerReloadIntegrationTests
  extends SimpleIOSuite
  with LanguageServerIntegrationTests {

  private def readBytes(path: Path): IO[Array[Byte]] = Files[IO]
    .readAll(path)
    .compile
    .to(Array)

  private def write(path: Path, text: String): IO[Unit] = writeBytes(path, text.getBytes)

  private def writeBytes(path: Path, bytes: Array[Byte]): IO[Unit] =
    fs2
      .Stream
      .emits(bytes)
      .through(Files[IO].writeAll(path))
      .compile
      .drain

  test("workspace reload results in code lenses showing up") {

    Files[IO]
      .tempDirectory
      .evalTap { base =>
        write(base / "smithy-build.json", "{}") *>
          write(
            base / "demo.smithyql",
            """use service weather#WeatherService
              |GetWeather { city: "hello" }""".stripMargin,
          )
      }
      .flatMap { base =>
        makeServer(Uri.fromPath(base))
          .evalMap { f =>
            val getLenses = f
              .server
              .codeLens(
                new CodeLensParams(
                  new TextDocumentIdentifier((f.workspaceDir / "demo.smithyql").value)
                )
              )

            val workspacePath =
              Path("modules") /
                "lsp" /
                "src" /
                "test" /
                "resources" /
                "test-workspace"

            val weatherPath = workspacePath / "weather.smithy"

            val addLibrary = readBytes(workspacePath / "smithy-build.json")
              .flatMap { bytes =>
                BuildConfigDecoder.decode(bytes).liftTo[IO]
              }
              .flatMap { baseConfig =>
                writeBytes(
                  base / "smithy-build.json",
                  BuildConfigDecoder.encode(
                    baseConfig.copy(
                      imports = weatherPath.absolute.toString :: Nil
                    )
                  ),
                )
              }

            getLenses.flatMap { lensesBefore =>
              assert.same(lensesBefore, Nil).failFast[IO]
            } *>
              addLibrary *>
              f.server.didChangeWatchedFiles(new DidChangeWatchedFilesParams()) *>
              getLenses
          }
      }
      .use { lensesAfter =>
        assert.same(lensesAfter.length, 1).pure[IO]
      }
  }

  test("workspace reload works even if there initially was no config file") {
    Files[IO]
      .tempDirectory
      .flatMap { base =>
        makeServer(Uri.fromPath(base))
          .evalMap { f =>
            f.client.scoped {
              writeBytes(
                base / "smithy-build.json",
                BuildConfigDecoder.encode(BuildConfig()),
              ) *>
                f.server.didChangeWatchedFiles(new DidChangeWatchedFilesParams())
            }
          }
      }
      .use_
      .as(success)
  }
}
