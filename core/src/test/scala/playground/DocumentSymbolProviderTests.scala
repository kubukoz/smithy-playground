package playground

import weaver._
import playground.smithyql.SourceRange
import playground.smithyql.Position

object DocumentSymbolProviderTests extends FunSuite {

  def makeDSL(documentText: String) = new DocumentDSL(documentText)

  final class DocumentDSL(documentText: String) {

    def symbols = DocumentSymbolProvider.make(documentText)

    def textRange(text: String): SourceRange = {
      val i = documentText.indexOf(text)
      SourceRange(
        Position(i),
        Position(i + text.length),
      )
    }

    def symbol(
      name: String,
      kind: SymbolKind,
      selectionRangeText: String,
      rangeText: String,
      children: List[DocumentSymbol] = Nil,
    ): DocumentSymbol = DocumentSymbol(
      name,
      kind,
      selectionRange = textRange(selectionRangeText),
      range = textRange(rangeText),
      children,
    )

  }

  test("hello world with one field") {
    val dsl = makeDSL("""hello { greeting = 42 }""")
    val result = dsl.symbols
    val expected = List(
      dsl.symbol(
        "hello",
        SymbolKind.Function,
        selectionRangeText = "hello",
        rangeText = "hello { greeting = 42 ",
        List(
          dsl.symbol(
            "greeting",
            SymbolKind.Field,
            selectionRangeText = "greeting",
            rangeText = "greeting = 42",
          )
        ),
      )
    )

    assert(result == expected)
  }
}
