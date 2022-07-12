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
import java.util.UUID
import demo.smithy.Ints
import demo.smithy.IntSet
import demo.smithy.MyInt
import demo.smithy.MyString
import demo.smithy.PowerMap
import demo.smithy.HasNewtypes
import demo.smithy.HasDeprecations
import smithy4s.Hints

object CompletionTests extends FunSuite {

  def getCompletions(
    schema: Schema[_],
    ctx: List[WithSource.NodeContext.PathEntry],
  ): List[CompletionItem] = schema.compile(CompletionVisitor).getCompletions(ctx)

  test("completions on struct are empty without StructBody") {

    val completions = getCompletions(Good.schema, Nil)

    assert(completions.isEmpty)
  }

  test("completions on struct include all field names") {

    val completions = getCompletions(Good.schema, List(StructBody))

    val fieldNames = completions.map(_.label)

    assert.eql(fieldNames, List("howGood")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Field))
  }

  test("completions on struct describe the field types") {

    val completions = getCompletions(Good.schema, List(StructBody))

    val results = completions.map { field =>
      (field.label, field.detail)
    }

    assert.eql(results, List("howGood" -> ": integer Integer"))
  }

  test("completions on struct add prefix/docs for optional fields") {
    val AnOptionalFieldLabel = "str"

    val completions = getCompletions(HasNewtypes.schema, List(StructBody))
      .filter(_.label == AnOptionalFieldLabel)

    val details = completions.map(_.detail)
    val docs = completions.map(_.docs)

    assert.eql(details, List("?: string MyString")) &&
    assert.eql(docs, List("**Optional**".some))
  }

  test("completions on union are empty without StructBody") {

    val completions = getCompletions(Hero.schema, Nil)

    assert(completions.isEmpty)
  }

  test("completions on union") {

    val completions = getCompletions(Hero.schema, List(StructBody))

    val fieldNames = completions.map(_.label)
    val details = completions.map(_.detail)
    val kinds = completions.map(_.kind)

    assert.eql(fieldNames, List("good", "bad", "badder")) &&
    assert(details == List(": structure Good", ": structure Bad", ": structure Bad")) &&
    assert(kinds.forall(_ == CompletionItemKind.UnionMember))
  }

  test("completions on union case are the same as completions on the underlying structure") {
    val pathToField = List(StructValue("good"), StructBody)

    val completionsOnAlt = getCompletions(
      Hero.schema,
      StructBody :: pathToField,
    ).map(_.label)

    val completionsOnStruct = getCompletions(
      Good.schema,
      StructBody :: Nil,
    ).map(_.label)

    assert.eql(completionsOnAlt, completionsOnStruct)
  }

  test("no completions on collection without entry") {
    val completions = getCompletions(
      Schema.list(Good.schema),
      Nil,
    )

    assert(completions.isEmpty)
  }

  test("completions on struct in list are available") {
    val completions = getCompletions(
      Schema.list(Good.schema),
      List(CollectionEntry(Some(0)), StructBody),
    )

    val fieldNames = completions.map(_.label)

    assert.eql(fieldNames, List("howGood"))
  }

  test("completions on enum without quotes have quotes") {

    val completions = getCompletions(Power.schema, Nil)

    val inserts = completions.map(_.insertText)
    val expectedInserts = List("Ice", "Fire", "Lightning", "Wind")
      .map(s => s"\"$s\"")
      .map(InsertText.JustString(_))

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.EnumMember)) &&
    assert(inserts == expectedInserts)
  }

  test("completions on enum in quotes don't have quotes") {
    val completions = getCompletions(Power.schema, List(Quotes))

    val inserts = completions.map(_.insertText)
    val expectedInserts = List("Ice", "Fire", "Lightning", "Wind")
      .map(InsertText.JustString(_))

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.EnumMember)) &&
    assert(inserts == expectedInserts)
  }

  test("completions on enum don't have Optional docs") {
    val completions = getCompletions(Power.schema, List(Quotes))

    val docs = completions.flatMap(_.docs)

    assert(docs.isEmpty)
  }

  test("completions on map keys that are enums") {
    val completions = getCompletions(PowerMap.schema, List(StructBody))

    val inserts = completions.map(_.insertText)

    val expectedInserts = List("Ice", "Fire", "Lightning", "Wind")
      .map(_ + " = ")
      .map(InsertText.JustString(_))

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.EnumMember)) &&
    assert(inserts == expectedInserts)
  }

  test("completions on map values (struct)") {
    val completions = getCompletions(
      Schema
        .map(
          Schema.string,
          Good.schema,
        ),
      List(
        StructBody,
        StructValue("anyKey"),
        StructBody,
      ),
    )

    val fieldNames = completions.map(_.label)

    assert.eql(fieldNames, List("howGood")) &&
    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Field))
  }

  test("completions on timestamp without quotes have quotes") {
    val completions = getCompletions(Schema.timestamp, Nil)

    val extractQuote = """\"(.*)\"""".r

    val inserts = completions.map(_.insertText).foldMap {
      case JustString(extractQuote(value)) =>
        assert(Timestamp.parse(value, TimestampFormat.DATE_TIME).isDefined)
      case s => failure("unexpected insert text: " + s)
    }

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Constant)) &&
    assert.eql(completions.size, 1) &&
    inserts
  }

  test("completions on timestamp in quotes don't have quotes") {
    val completions = getCompletions(
      Schema.timestamp,
      List(Quotes),
    )

    val inserts = completions.map(_.insertText).foldMap {
      case JustString(value) => assert(Timestamp.parse(value, TimestampFormat.DATE_TIME).isDefined)
      case s                 => failure("unexpected insert text: " + s)
    }

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Constant)) &&
    assert.eql(completions.size, 1) &&
    inserts
  }

  test("completions on uuid include a random uuid") {
    val completions = getCompletions(Schema.uuid, List(Quotes))

    val inserts = completions.map(_.insertText).foldMap {
      case JustString(value) =>
        val parsed = Either.catchNonFatal(UUID.fromString(value))
        assert(parsed.isRight)

      case s => failure("unexpected insert text: " + s)
    }

    assert(completions.map(_.kind).forall(_ == CompletionItemKind.Constant)) &&
    assert.eql(completions.size, 1) &&
    inserts
  }

  test("completions on deprecated fields have proper hints in docs") {
    val completions = getCompletions(HasDeprecations.schema, List(StructBody)).filter(_.deprecated)

    val results = completions.map(c => (c.label, c.docs)).toMap

    assert.eql(
      results,
      Map(
        "hasBoth" -> Some("**Deprecated** (since 1.0.0): Another reason"),
        "hasMessage" -> Some("**Deprecated**: Made-up reason"),
        "hasSince" -> Some("**Deprecated** (since 0.1.0)"),
      ),
    )
  }

  test("describe indexed seq") {
    assert.eql(
      CompletionItem.describeSchema(Ints.schema)(),
      "@indexedSeq list Ints { member: integer Integer }",
    )
  }

  test("describe set of ints") {
    assert.eql(
      CompletionItem.describeSchema(IntSet.schema)(),
      "set IntSet { member: integer Integer }",
    )
  }

  test("describe int newtype") {
    assert.eql(
      CompletionItem.describeSchema(MyInt.schema)(),
      "integer MyInt",
    )
  }

  test("describe string newtype") {
    assert.eql(
      CompletionItem.describeSchema(MyString.schema)(),
      "string MyString",
    )
  }

  test("describe enum") {
    assert.eql(
      CompletionItem.describeSchema(Power.schema)(),
      "enum Power",
    )
  }

  test("describe map") {
    assert.eql(
      CompletionItem.describeSchema(PowerMap.schema)(),
      "map PowerMap { key: Power, value: Hero }",
    )
  }

  test("describe uuid") {
    assert.eql(
      CompletionItem.describeSchema(Schema.uuid)(),
      "uuid UUID",
    )
  }

  test("describe non-field: no optionality sign") {
    assert.eql(
      CompletionItem.describeType(isField = false, Schema.string),
      ": string String",
    )
  }

  test("describe required field: no optionality sign") {
    assert.eql(
      CompletionItem.describeType(isField = true, Schema.string.addHints(smithy.api.Required())),
      ": string String",
    )
  }

  test("describe optional field: optionality sign present") {
    assert.eql(
      CompletionItem.describeType(isField = true, Schema.string),
      "?: string String",
    )
  }

  test("buildDocumentation: deprecation note goes before optionality note") {
    val doc = CompletionItem.buildDocumentation(
      isField = true,
      hints = Hints(smithy.api.Deprecated()),
    )

    assert.eql(
      doc,
      """**Deprecated**
        |
        |**Optional**""".stripMargin.some,
    )
  }

}
