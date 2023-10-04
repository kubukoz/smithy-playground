package playground.smithyql.parser.v2

import playground.smithyql.parser.v2.scanner.TokenKind._
import weaver._

object ScannerExampleTests extends SimpleIOSuite with ScannerSuite {
  scanTest(
    explicitName = "Real file 1",
    input =
      """use service demo.smithy#DemoService
        |
        |// CreateSubscription {
        |//   subscription: {
        |//     id: "subscription_id",
        |//     name: "name",
        |//     createdAt: "2020-04-01T00:00:00Z",
        |//   },
        |// }
        |CreateHero {
        |  hero: {
        |    good: // bgasdfasldf
        |      {
        |        howGood: 10,
        |      },
        |  },
        |  intSet: [
        |    1,
        |    2,
        |    1,
        |  ],
        |}
        |""".stripMargin,
  )(
    List(
      KW_USE("use"),
      SPACE(" "),
      KW_SERVICE("service"),
      SPACE(" "),
      IDENT("demo"),
      DOT("."),
      IDENT("smithy"),
      HASH("#"),
      IDENT("DemoService"),
      NEWLINE("\n\n"),
      COMMENT("// CreateSubscription {"),
      NEWLINE("\n"),
      COMMENT("//   subscription: {"),
      NEWLINE("\n"),
      COMMENT("//     id: \"subscription_id\","),
      NEWLINE("\n"),
      COMMENT("//     name: \"name\","),
      NEWLINE("\n"),
      COMMENT("//     createdAt: \"2020-04-01T00:00:00Z\","),
      NEWLINE("\n"),
      COMMENT("//   },"),
      NEWLINE("\n"),
      COMMENT("// }"),
      NEWLINE("\n"),
      IDENT("CreateHero"),
      SPACE(" "),
      LBR("{"),
      NEWLINE("\n"),
      SPACE("  "),
      IDENT("hero"),
      COLON(":"),
      SPACE(" "),
      LBR("{"),
      NEWLINE("\n"),
      SPACE("    "),
      IDENT("good"),
      COLON(":"),
      SPACE(" "),
      COMMENT("// bgasdfasldf"),
      NEWLINE("\n"),
      SPACE("      "),
      LBR("{"),
      NEWLINE("\n"),
      SPACE("        "),
      IDENT("howGood"),
      COLON(":"),
      SPACE(" "),
      LIT_NUMBER("10"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("      "),
      RBR("}"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("  "),
      RBR("}"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("  "),
      IDENT("intSet"),
      COLON(":"),
      SPACE(" "),
      LB("["),
      NEWLINE("\n"),
      SPACE("    "),
      LIT_NUMBER("1"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("    "),
      LIT_NUMBER("2"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("    "),
      LIT_NUMBER("1"),
      COMMA(","),
      NEWLINE("\n"),
      SPACE("  "),
      RB("]"),
      COMMA(","),
      NEWLINE("\n"),
      RBR("}"),
      NEWLINE("\n"),
    )
  )
}
