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

val also = q
  .input
  .fields
  .value
  .value
  .toList
  .collectFirst { case (k, v) if k.value.text == "hero" => v }
  .get
  .asInstanceOf[Struct[WithSource]]
  .fields
  .value
  .value
  .toList
  .collectFirst { case (k, v) if k.value.text == "bad" => v }
  .get
  .asInstanceOf[Struct[WithSource]]

def show(range: SourceRange) = raw.substring(range.start.index, range.end.index)

show(also.fields.value.range)
