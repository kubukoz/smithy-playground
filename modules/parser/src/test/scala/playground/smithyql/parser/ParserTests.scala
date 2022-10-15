package playground.smithyql.parser

import cats.effect.IO
import cats.implicits._
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.Codec
import playground.smithyql._
import playground.smithyql.parser.SmithyQLParser
import weaver._

import java.nio.file.Paths
import playground.Assertions._
import Diffs._

object ParserTests extends SimpleIOSuite {

  implicit val queryWS: Codec[Query[WithSource]] = {
    import io.circe.generic.auto._

    io.circe.generic.semiauto.deriveCodec
  }

  private def readText(path: Path) =
    Files[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .compile
      .string

  val baseDir = Paths.get(getClass().getResource("/parser-examples").getFile())

  baseDir.toFile.list().filter(baseDir.resolve(_).toFile().isDirectory()).foreach { testName =>
    val testBase = Path.fromNioPath(baseDir) / testName

    test(testName) { (_, l) =>
      implicit val log = l

      val inputIO = readText(testBase / "input.smithyql-test")

      val outputIO = readText(testBase / "output.json")
        .flatMap(io.circe.parser.decode[Query[WithSource]](_).liftTo[IO])

      (inputIO, outputIO).mapN { (input, expected) =>
        SmithyQLParser.parseFull(input) match {
          case Left(e)  => failure(s"Parsing failed: ${e.msg}").pure[IO]
          case Right(v) => assertNoDiff(v, expected)
        }
      }.flatten
    }
  }

}
