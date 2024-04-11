package playground.smithyql.parser.v2

import cats.effect.IO
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import playground.Assertions
import playground.smithyql.parser.v2.scanner.Scanner
import playground.smithyql.parser.v2.scanner.Token
import weaver.*

import Diffs.*
import Scanner.scan

trait ScannerSuite { self: IOSuite =>

  protected def arbTests(
    name: TestName
  )(
    withArb: Arbitrary[String] => IO[Expectations]
  ): Unit = {

    val sampleStringGen = Gen.oneOf(
      Gen.alphaStr,
      Gen.alphaNumStr,
      Gen.asciiPrintableStr,
      Gen.identifier,
      Gen.oneOf(List(' ', '\n', '\t', '\r', '\f', '\b')).map(_.toString),
    )

    val arbString: Arbitrary[String] = Arbitrary {
      Gen.listOf(sampleStringGen).map(_.mkString)
    }

    test(name)(withArb(Arbitrary.arbString))
    test(name.copy(name = name.name + " (prepared input)"))(withArb(arbString))
  }

  protected def scanTest(
    input: String,
    explicitName: TestName = "",
  )(
    expected: List[Token]
  )(
    implicit loc: SourceLocation
  ): Unit =
    pureTest(
      if (explicitName.name.nonEmpty)
        explicitName
      else
        "Scan string: " + sanitize(input)
    ) {
      Assertions.assertNoDiff(scan(input), expected)
    }

  // Runs scanTest by first rendering the expected tokens to a string, then scanning it to get them back.
  // If the output is not the same as the input, the test fails.
  // While it's guaranteed that rendering tokens to text produces scannable code (everything is scannable),
  // due to ambiguities in the scanner it's not guaranteed that the output will be the same as the input - hence the need to test.
  protected def scanTestReverse(
    explicitName: String
  )(
    expected: List[Token]
  )(
    implicit loc: SourceLocation
  ): Unit = scanTest(expected.foldMap(_.text), explicitName)(expected)

  protected def sanitize(
    text: String
  ) = text.replace(" ", "·").replace("\n", "↵")

}
