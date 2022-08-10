package playground

import com.softwaremill.diffx.generic.auto._
import playground.smithyql.Position
import playground.smithyql.SourceRange
import weaver._

import Assertions._

object DocumentSymbolProviderTests extends SimpleIOSuite {

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

  test("hello world with one field") { (_, log) =>
    implicit val l = log
    val dsl = makeDSL("""hello { greeting = 42 }""")
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

    assertNoDiff(dsl.symbols, expected)
  }

  test("nesting in structs") { (_, log) =>
    implicit val l = log
    val dsl = makeDSL("""q { a = { b = { c = { d = 42, }, }, }, }""")

    val expected = List(
      dsl.symbol(
        "q",
        SymbolKind.Function,
        selectionRangeText = "q",
        rangeText = "q { a = { b = { c = { d = 42, }, }, }, ",
        List(
          dsl.symbol(
            "a",
            SymbolKind.Field,
            selectionRangeText = "a",
            rangeText = "a = { b = { c = { d = 42, }, }, }",
            List(
              dsl.symbol(
                "b",
                SymbolKind.Field,
                selectionRangeText = "b",
                rangeText = "b = { c = { d = 42, }, }",
                List(
                  dsl.symbol(
                    "c",
                    SymbolKind.Field,
                    selectionRangeText = "c",
                    rangeText = "c = { d = 42, }",
                    List(
                      dsl.symbol(
                        "d",
                        SymbolKind.Field,
                        selectionRangeText = "d",
                        rangeText = "d = 42",
                      )
                    ),
                  )
                ),
              )
            ),
          )
        ),
      )
    )

    assertNoDiff(dsl.symbols, expected)
  }

}
