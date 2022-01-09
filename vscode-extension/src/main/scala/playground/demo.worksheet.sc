import demo.smithy.DemoServiceGen

import playground.CompletionSchematic

import playground.smithyql.SourceRange

import playground.smithyql.WithSource

import playground.smithyql.Struct

import playground.smithyql.SmithyQLParser

val raw = """
CreateHero {
  hero = {
    bad = {

      // evilName = "Devil",
      // powerLevel = 420,

    },
  },
}
"""

val q =
  SmithyQLParser
    .parseFull(raw)
    .toTry
    .get

DemoServiceGen.CreateHero.input.compile(new CompletionSchematic()).apply("hero" :: Nil)
