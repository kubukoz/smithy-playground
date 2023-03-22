package playground.smithyql.parser

import cats.effect.IO
import cats.implicits._
import com.softwaremill.diffx.Diff
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.Codec
import io.circe.Decoder
import io.circe.syntax._
import playground.Assertions._
import playground.smithyql._
import weaver._

import java.nio.file
import java.nio.file.NoSuchFileException
import java.nio.file.Paths

trait ParserSuite extends SimpleIOSuite {

  def loadParserTests[Alg[_[_]]: SourceParser](
    prefix: String,
    // this isn't on by default because whitespace in full files (currently, 1-1 mapping to queries) is significant and should not be trimmed before parsing.
    trimWhitespace: Boolean = false,
  )(
    implicit codec: Codec[Alg[WithSource]],
    diff: Diff[Alg[WithSource]],
  ): Unit = loadTestCases(".json", List(prefix)).foreach { testCase =>
    test(testCase.name) {

      testCase.readInput(trimWhitespace).flatMap { input =>
        ParserSuite.assertParses[Alg](input) match {
          case Left(e) => failure(e).pure[IO]

          case Right(v) =>
            testCase
              .readOutput[Alg[WithSource]]
              .map { expected =>
                assertNoDiff(v, expected)
              }
              .recoverWith { case _: NoSuchFileException =>
                testCase
                  .writeOutput(
                    v.asJson.spaces2
                  )
                  .map { outputPath =>
                    failure(
                      s"Test ${testCase.name} didn't have an expected output file, so it was created. Run it again. Output path: $outputPath"
                    )
                  }
              }
        }
      }
    }
  }

  def loadNegativeParserTests[Alg[_[_]]: SourceParser](
    prefix: String,
    trimWhitespace: Boolean = false,
  ): Unit = loadTestCases("", List("negative", prefix)).foreach { testCase =>
    test(testCase.name) {
      testCase.readInput(trimWhitespace).map { input =>
        SourceParser[Alg].parse(input) match {
          case Left(_)  => success
          case Right(_) => failure("Expected parsing failure but got a success instead")
        }
      }
    }
  }

  private def readText(
    path: Path
  ) =
    Files[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .compile
      .string

  private def writeText(
    path: Path,
    text: String,
  ) =
    fs2
      .Stream
      .emit(text)
      .through(fs2.text.utf8.encode[IO])
      .through(Files[IO].writeAll(path))
      .compile
      .drain

  private def loadTestCases(
    outputExtension: String,
    prefix: List[String],
  ): List[TestCase] = {
    val baseDir = Paths
      .get(getClass().getResource("/parser-examples").getFile())
      .resolve(file.Path.of(prefix.head, prefix.tail: _*))

    baseDir
      .toFile
      .list()
      .filter(baseDir.resolve(_).toFile().isDirectory())
      .map { testName =>
        val testBase = Path.fromNioPath(baseDir) / testName

        TestCase(
          name = testName,
          base = testBase,
          outputExtension = outputExtension,
        )
      }
      .toList
  }

  private case class TestCase(
    name: String,
    base: Path,
    outputExtension: String,
  ) {

    private val outputPath = base / s"output$outputExtension"

    def readInput(
      trimWhitespace: Boolean
    ): IO[String] = readText(base / "input.smithyql-test").map(
      if (trimWhitespace)
        _.strip
      else
        identity
    )

    def readOutput[Out: Decoder]: IO[Out] = readText(outputPath)
      .flatMap(
        io.circe
          .parser
          .decode[Out](_)
          // this wrapping can be skipped in 0.15.x
          .leftMap(de => new Throwable(de.show))
          .liftTo[IO]
      )

    def writeOutput(
      text: String
    ): IO[Path] = writeText(outputPath, text).as(outputPath)

  }

}

object ParserSuite {

  def assertParses[Alg[_[_]]: SourceParser](
    s: String
  ) = SourceParser[Alg]
    .parse(s)
    .leftMap(e => s"Parsing failed: \n==========\n${e.debug}\n==========")

}
