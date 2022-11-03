package playground.language

import cats.Id
import cats.implicits._
import fs2.io.file.Path
import playground.smithyql.Position
import playground.smithyql.SourceRange
import weaver._

object FormattingProviderTests extends FunSuite {
  private val anyUri = Uri.fromPath(Path("file.smithyql"))

  private def provideForFile(s: String) = {
    implicit val tdp: TextDocumentProvider[Id] = TextDocumentProvider.always[Id](
      s
    )

    FormattingProvider.provider[Id](80).apply(anyUri)
  }

  test("formatting empty file") {
    assert.same(
      provideForFile(""),
      List(
        TextEdit.Overwrite("", SourceRange.empty(Position.origin))
      ),
    )
  }

  test("formatting file with just whitespace") {
    assert.same(
      provideForFile("    \n\n   "),
      List(
        TextEdit.Overwrite("", SourceRange.forEntireString("    \n\n   "))
      ),
    )
  }

  test("formatting non-parsing file") {
    assert.same(provideForFile("!@(*$^%@&*"), Nil)
  }

  test("formatting a parseable file") {
    assert.same(
      provideForFile("hello {}"),
      List(
        TextEdit.Overwrite(
          """hello {
            |
            |}
            |""".stripMargin,
          SourceRange.forEntireString("hello {}"),
        )
      ),
    )
  }
}
