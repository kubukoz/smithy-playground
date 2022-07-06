package playground.smithyql

import weaver._
import demo.smithy.Good
import smithy4s.schema.Schema

object CompletionTests extends FunSuite {
  test("completions on struct include all field names") {

    val completions = Good.schema.compile(new CompletionSchematic).get.apply(Nil)

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("howGood")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Field))
  }

  test("completions on struct in list are available") {
    val completions = Schema
      .list(Good.schema)
      .compile(new CompletionSchematic)
      .get
      .apply(List(WithSource.NodeContext.PathEntry.CollectionEntry))

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("howGood"))
  }
}
