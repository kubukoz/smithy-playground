package playground.smithyql.parser

import cats.effect.IO
import cats.implicits._
import fs2.io.file.Files
import fs2.io.file.Path
import playground.smithyql._
import playground.smithyql.parser.SmithyQLParser
import weaver._

import java.nio.file.Paths
import playground.Assertions._
import Diffs._
import io.circe.syntax._
import java.nio.file.NoSuchFileException
import Codecs._

object ParserTests extends SimpleIOSuite {

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

  val baseDir = Paths.get(getClass().getResource("/parser-examples").getFile())

  baseDir.toFile.list().filter(baseDir.resolve(_).toFile().isDirectory()).foreach { testName =>
    val testBase = Path.fromNioPath(baseDir) / testName
    val outputPath = testBase / "output.json"

    test(testName) {

      val inputIO = readText(testBase / "input.smithyql-test")

      val outputIO = readText(outputPath)
        .flatMap(io.circe.parser.decode[Query[WithSource]](_).liftTo[IO])

      inputIO.flatMap { input =>
        SmithyQLParser.parseFull(input) match {
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
