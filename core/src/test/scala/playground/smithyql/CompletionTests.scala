package playground.smithyql

import weaver._
import demo.smithy.Good
import smithy4s.schema.Schema
import playground.smithyql.WithSource.NodeContext.PathEntry._
import demo.smithy.Hero
import demo.smithy.Power
import smithy4s.Timestamp
import playground.smithyql.InsertText.JustString
import smithy.api.TimestampFormat
import cats.implicits._

object CompletionTests extends FunSuite {
  test("completions on struct are empty without StructBody") {

    val completions = Good
      .schema
      .compile(CompletionVisitor)
      .getCompletions(Nil)

    assert(completions.isEmpty)
  }

  test("completions on struct include all field names") {

    val completions = Good
      .schema
      .compile(CompletionVisitor)
      .getCompletions(List(StructBody))

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("howGood")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Field))
  }

  test("completions on union are empty without StructBody") {

    val completions = Hero
      .schema
      .compile(CompletionVisitor)
      .getCompletions(Nil)

    assert(completions.isEmpty)
  }

  test("completions on union include all alt names") {

    val completions = Hero
      .schema
      .compile(CompletionVisitor)
      .getCompletions(List(StructBody))

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("good", "bad", "badder")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.UnionMember))
  }

  test("completions on union case are the same as completions on the underlying structure") {

    val completionsOnAlt = Hero
      .schema
      .compile(CompletionVisitor)
      .getCompletions(List(StructBody, StructValue("good"), StructBody))
      .map(_.label)

    val completionsOnStruct = Good
      .schema
      .compile(CompletionVisitor)
      .getCompletions(List(StructBody))
      .map(_.label)

    assert(completionsOnAlt == completionsOnStruct)
  }

  test("no completions on collection without entry") {
    val completions = Schema
      .list(Good.schema)
      .compile(CompletionVisitor)
      .getCompletions(Nil)

    assert(completions.isEmpty)
  }

  test("completions on struct in list are available") {
    val completions = Schema
      .list(Good.schema)
      .compile(CompletionVisitor)
      .getCompletions(
        List(
          CollectionEntry(Some(0)),
          StructBody,
        )
      )

    val fieldNames = completions.map(_.label)

    assert(fieldNames == List("howGood"))
  }

  test("completions on enum without quotes have quotes") {
    val completions = Power
      .schema
      .compile(CompletionVisitor)
      .getCompletions(Nil)

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
      .compile(CompletionVisitor)
      .getCompletions(List(Quotes))

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
      .compile(CompletionVisitor)
      .getCompletions(List(StructBody))

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
      .compile(CompletionVisitor)
      .getCompletions(
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

  test("completions on timestamp without quotes have quotes") {
    val completions = Schema
      .timestamp
      .compile(CompletionVisitor)
      .getCompletions(Nil)

    val extractQuote = """\"(.*)\"""".r

    val inserts = completions.map(_.insertText).foldMap {
      case JustString(extractQuote(value)) =>
        assert(Timestamp.parse(value, TimestampFormat.DATE_TIME).isDefined)
      case s => failure("unexpected insert text: " + s)
    }

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Constant))
    && inserts
  }

  test("completions on timestamp in quotes don't have quotes") {
    val completions = Schema
      .timestamp
      .compile(CompletionVisitor)
      .getCompletions(List(Quotes))

    val inserts = completions.map(_.insertText).foldMap {
      case JustString(value) => assert(Timestamp.parse(value, TimestampFormat.DATE_TIME).isDefined)
      case s                 => failure("unexpected insert text: " + s)
    }

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Constant))
    && inserts
  }

}
