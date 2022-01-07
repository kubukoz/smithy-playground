import playground.smithyql.WithSource

import playground.smithyql.Struct

import playground.smithyql.SmithyQLParser

val raw = """
Hello {
  my = "dear world",
  also = {
    this = "is awesome",
    andThisIs = 42
  }
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
  .collectFirst { case (k, v) if k.value.text == "also" => v }
  .get
  .asInstanceOf[Struct[WithSource]]

raw.drop(also.fields.position.start.index)

raw.substring(also.fields.position.start.index, also.fields.position.end.index)

q.input.fields.position
