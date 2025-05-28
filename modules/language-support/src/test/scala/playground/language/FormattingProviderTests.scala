package playground.language

import cats.Id
import fs2.io.file.Path
import playground.smithyql.Position
import playground.smithyql.SourceRange
import weaver.*

object FormattingProviderTests extends FunSuite {
  private val anyUri = Uri.fromPath(Path("file.smithyql"))

  private def provideForFile(
    s: String
  ) = {
    implicit val tdp: TextDocumentProvider[Id] = TextDocumentProvider.always[Id](
      s
    )

    FormattingProvider.provider[Id](80).apply(anyUri)
  }

  test("formatting empty file") {
    expect.same(
      provideForFile(""),
      List(
        TextEdit.Overwrite("\n", SourceRange.empty(Position.origin))
      ),
    )
  }

  test("formatting file with just whitespace") {
    expect.same(
      provideForFile("    \n\n   "),
      List(
        TextEdit.Overwrite("\n", SourceRange.forEntireString("    \n\n   "))
      ),
    )
  }

  test("formatting non-parsing file") {
    expect.same(provideForFile("!@(*$^%@&*"), Nil)
  }

  test("formatting a parseable file") {
    expect.same(
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
