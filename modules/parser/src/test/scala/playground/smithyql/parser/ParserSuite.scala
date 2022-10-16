package playground.smithyql.parser

import cats.effect.IO
import cats.implicits._
import com.softwaremill.diffx.Diff
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.Codec
import io.circe.syntax._
import playground.Assertions._
import playground.smithyql._
import weaver._

import java.nio.file.NoSuchFileException
import java.nio.file.Paths

trait ParserSuite extends SimpleIOSuite {

  private def readText(path: Path) =
    Files[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .compile
      .string

  private def writeText(path: Path, text: String) =
    fs2
      .Stream
      .emit(text)
      .through(fs2.text.utf8.encode[IO])
      .through(Files[IO].writeAll(path))
      .compile
      .drain

  def loadParserTests[Alg[_[_]]: SourceParser](
    prefix: String,
    // this isn't on by default because whitespace in full files (currently, 1-1 mapping to queries) is significant and should not be trimmed before parsing.
    trimWhitespace: Boolean = false,
  )(
    implicit codec: Codec[Alg[WithSource]],
    diff: Diff[Alg[WithSource]],
  ) = {
    val baseDir = Paths.get(getClass().getResource("/parser-examples").getFile()).resolve(prefix)

    baseDir
      .toFile
      .list()
      .filter(baseDir.resolve(_).toFile().isDirectory())
      .foreach { testName =>
        val testBase = Path.fromNioPath(baseDir) / testName
        val outputPath = testBase / "output.json"

        test(testName) {

          val inputIO = readText(testBase / "input.smithyql-test").map(
            if (trimWhitespace)
              _.strip
            else
              identity
          )

          val outputIO = readText(outputPath)
            .flatMap(io.circe.parser.decode[Alg[WithSource]](_).liftTo[IO])

          inputIO.flatMap { input =>
            SourceParser[Alg].parse(input) match {
              case Left(e) => failure(s"Parsing failed: ${e.msg}").pure[IO]
              case Right(v) =>
                outputIO
                  .map { expected =>
                    assertNoDiff(v, expected)
                  }
                  .recoverWith { case _: NoSuchFileException =>
                    writeText(
                      outputPath,
                      v.asJson.spaces2,
                    ).as(
                      failure(
                        s"Test $testName didn't have an expected output file, so it was created. Run it again. Output path: $outputPath"
                      )
                    )
                  }
            }
          }
        }
      }
  }

}
