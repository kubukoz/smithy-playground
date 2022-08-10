package playground

import weaver._
import playground.smithyql.SourceRange
import playground.smithyql.Position

object DocumentSymbolProviderTests extends FunSuite {

  private def symbols(s: String) = DocumentSymbolProvider.make(s)

  test("hello world with one field") {
    val result = symbols("""hello { greeting = 42 }""")
    val expected = List(
      DocumentSymbol(
        "hello",
        SymbolKind.Function,
        SourceRange(Position(0), Position("hello".length)),
        SourceRange(Position(0), Position("hello { greeting = 42 ".length)),
        List(
          DocumentSymbol(
            "greeting",
            SymbolKind.Field,
            SourceRange(Position("hello { ".length), Position("hello { greeting".length)),
            SourceRange(Position("hello { ".length), Position("hello { greeting = 42".length)),
            Nil,
          )
        ),
      )
    )

    assert(result == expected)
  }
}
