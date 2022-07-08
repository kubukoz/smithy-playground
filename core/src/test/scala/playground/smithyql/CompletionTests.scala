package playground.smithyql

import weaver._
import demo.smithy.Good
import smithy4s.schema.Schema
import playground.smithyql.WithSource.NodeContext.PathEntry._
import demo.smithy.Hero
import demo.smithy.Power

object CompletionTests extends FunSuite {
  test("completions on struct are empty without StructBody") {

    val completions = Good
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(Nil)

    assert(completions.isEmpty)
  }

  test("completions on struct include all field names") {

    val completions = Good
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(List(StructBody))

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("howGood")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Field))
  }

  test("completions on union are empty without StructBody") {

    val completions = Hero
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(Nil)

    assert(completions.isEmpty)
  }

  test("completions on union include all alt names") {

    val completions = Hero
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(List(StructBody))

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("good", "bad", "badder")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.UnionMember))
  }

  test("completions on union case are the same as completions on the underlying structure") {

    val completionsOnAlt = Hero
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(List(StructBody, StructValue("good"), StructBody))
      .map(_.label)

    val completionsOnStruct = Good
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(List(StructBody))
      .map(_.label)

    assert(completionsOnAlt == completionsOnStruct)
  }

  test("completions on struct in list are available") {
    val completions = Schema
      .list(Good.schema)
      .compile(new CompletionSchematic)
      .get
      .apply(
        List(
          CollectionEntry,
          StructBody,
        )
      )

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("howGood"))
  }

  test("completions on enum without quotes have quotes") {
    val completions = Power
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(Nil)

    val inserts = completions.map(_.insertText)
    val expectedInserts = List("Ice", "Fire", "Lightning", "Wind")
      .map(s => s"\"$s\"")
      .map(InsertText.JustString(_))

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.EnumMember)) &&
    assert(inserts == expectedInserts)
  }

  test("completions on enum in quotes don't have quotes") {
    val completions = Power
      .schema
      .compile(new CompletionSchematic)
      .get
      .apply(List(Quotes))

    val inserts = completions.map(_.insertText)
    val expectedInserts = List("Ice", "Fire", "Lightning", "Wind")
      .map(InsertText.JustString(_))

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.EnumMember)) &&
    assert(inserts == expectedInserts)
  }

  test("completions on map keys that are enums") {
    val completions = Schema
      .map(
        Power.schema,
        Schema.unit,
      )
      .compile(new CompletionSchematic)
      .get
      .apply(List(StructBody))

    val inserts = completions.map(_.insertText)

    val expectedInserts = List("Ice", "Fire", "Lightning", "Wind")
      .map(_ + " = ")
      .map(InsertText.JustString(_))

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.EnumMember)) &&
    assert(inserts == expectedInserts)
  }

  test("completions on map values (struct)") {
    val completions = Schema
      .map(
        Schema.string,
        Good.schema,
      )
      .compile(new CompletionSchematic)
      .get
      .apply(
        List(
          StructBody,
          StructValue("anyKey"),
          StructBody,
        )
      )

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("howGood")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Field))
  }
  // todo quoted/unquoted timestamps
}
