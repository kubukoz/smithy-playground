package playground

import cats.Id
import demo.smithy.Good
import demo.smithy.Hero
import demo.smithy.Person
import demo.smithy.Power
import demo.smithy.SampleSparseList
import playground.NodeEncoder
import playground.smithyql.AST
import playground.smithyql.DSL.*
import playground.smithyql.Listed
import playground.smithyql.NullLiteral
import playground.smithyql.StringLiteral
import smithy.api.TimestampFormat
import smithy4s.Blob
import smithy4s.Document
import smithy4s.Timestamp
import smithy4s.schema.Schema
import weaver.*

object NodeEncoderTests extends FunSuite {

  def assertEncodes[A](
    schema: Schema[A],
    value: A,
    expected: AST[Id],
  )(
    implicit loc: SourceLocation
  ): Expectations = {
    val enc = NodeEncoder.derive(schema)

    assert.eql(enc.toNode(value), expected)
  }

  test("unit") {
    assertEncodes(Schema.unit, (), struct())
  }

  test("String") {
    assertEncodes(Schema.string, "test", "test")
  }

  test("int") {
    assertEncodes(Schema.int, 42, 42)
  }

  test("double") {
    assertEncodes(Schema.double, 420.2137d, 420.2137d)
  }

  test("simple struct") {
    assertEncodes(Good.schema, Good(42), struct("howGood" -> 42))
  }

  test("struct with optionals: defined") {
    assertEncodes(
      Person.schema,
      Person("My name", age = Some(42)),
      struct("name" -> "My name", "age" -> 42),
    )
  }

  test("struct with optionals: empty") {
    assertEncodes(
      Person.schema,
      Person("My name", age = None),
      struct("name" -> "My name"),
    )
  }

  test("enum") {
    assertEncodes(Power.schema, Power.ICE, "ICE")
  }

  test("union") {
    assertEncodes(Hero.schema, Hero.GoodCase(Good(42)), struct("good" -> struct("howGood" -> 42)))
  }

  test("timestamp") {
    assertEncodes(
      Schema.timestamp,
      Timestamp.parse("2022-07-11T17:42:28Z", TimestampFormat.DATE_TIME).get,
      "2022-07-11T17:42:28Z",
    )
  }

  test("blob") {
    assertEncodes(
      Schema.bytes,
      Blob("foo"),
      StringLiteral("Zm9v"),
    )
  }

  test("null document") {
    assertEncodes(
      Schema.document,
      Document.nullDoc,
      NullLiteral(),
    )
  }

  test("sparse list") {
    assertEncodes(
      SampleSparseList.schema,
      SampleSparseList(List(Some(1), None, Some(3))),
      Listed[Id](List(1, NullLiteral(), 3)),
    )
  }
}
