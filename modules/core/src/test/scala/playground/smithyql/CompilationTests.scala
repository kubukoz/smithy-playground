package playground.smithyql

import cats.Show
import cats.data.Chain
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.implicits._
import demo.smithy.Bad
import demo.smithy.DeprecatedServiceGen
import demo.smithy.FriendSet
import demo.smithy.Good
import demo.smithy.HasConstraintFields
import demo.smithy.HasDefault
import demo.smithy.HasDeprecations
import demo.smithy.Hero
import demo.smithy.IntSet
import demo.smithy.Ints
import demo.smithy.MyInstant
import demo.smithy.Person
import demo.smithy.Power
import demo.smithy.StringWithLength
import demo.smithy.HasMixin
import org.scalacheck.Arbitrary
import playground.CompilationError
import playground.CompilationErrorDetails
import playground.CompilationFailed
import playground.DiagnosticTag
import playground.QueryCompiler
import playground.QueryCompilerVisitor
import smithy.api
import smithy.api.TimestampFormat
import smithy4s.ByteArray
import smithy4s.Document
import smithy4s.Refinement
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.ShapeTag
import smithy4s.Timestamp
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.schema.Schema
import software.amazon.smithy.model.{Model => SModel}
import weaver._
import weaver.scalacheck.Checkers

import java.time
import java.util.UUID

import Arbitraries._
import playground.smithyql.parser.SourceParser

object CompilationTests extends SimpleIOSuite with Checkers {

  import DSL._

  def compile[A: smithy4s.Schema](
    in: QueryCompiler.WAST
  ) = implicitly[smithy4s.Schema[A]].compile(QueryCompilerVisitor.full).compile(in)

  def loadDynamicModel(): DynamicSchemaIndex = {
    val model = SModel
      .assembler()
      .discoverModels()
      .assemble()
      .unwrap()

    DynamicSchemaIndex.loadModel(model).toTry.get
  }

  val dynamicModel = loadDynamicModel()

  def dynamicSchemaFor[A: ShapeTag]: Schema[_] = {
    val shapeId = ShapeTag[A].id

    dynamicModel
      .getSchema(shapeId)
      .getOrElse(sys.error("missing model for shape " + shapeId))
  }

  // Kinda hacky, but does the job
  def asDocument[A](schema: Schema[A]): Schema[Document] = {
    val encoder = Document.Encoder.fromSchema(schema)
    val decoder = Document.Decoder.fromSchema(schema)

    Schema.RefinementSchema(
      schema,
      new Refinement[A, Document] {
        type Constraint = Unit

        val tag: ShapeTag[Unit] =
          new ShapeTag[Unit] {
            val id: ShapeId = Schema.unit.shapeId
            val schema: Schema[Unit] = Schema.unit
          }

        val constraint: Unit = ()

        def apply(a: A): Either[String, Document] = unsafe(a).asRight

        def from(b: Document): A = b.decode(decoder).toTry.get

        def unsafe(a: A): Document = encoder.encode(a)
      },
    )
  }

  pureTest("unit") {
    assert(
      compile {
        WithSource.liftId(struct().mapK(WithSource.liftId))
      }(Schema.unit).isRight
    )
  }

  pureTest("unit - doesn't accept string") {
    assert(
      compile {
        WithSource.liftId("test".mapK(WithSource.liftId))
      }(Schema.unit).isLeft
    )
  }

  pureTest("unit - doesn't accept struct with a field present") {
    assert(
      compile {
        WithSource.liftId(struct("test" -> 42).mapK(WithSource.liftId))
      }(Schema.unit).isBoth
    )
  }

  pureTest("string") {
    assert(
      compile {
        WithSource.liftId("foo".mapK(WithSource.liftId))
      }(Schema.string) == Ior.right("foo")
    )
  }

  pureTest("string with length constraint - fail") {
    assert(
      compile[StringWithLength] {
        WithSource.liftId("".mapK(WithSource.liftId))
      }.isLeft
    )
  }

  pureTest("string with length constraint - fail (dynamic)") {
    val dynamicStringSchema = dynamicSchemaFor[StringWithLength]

    val result = compile {
      WithSource.liftId("".mapK(WithSource.liftId))
    }(dynamicStringSchema)
      .leftMap(_.map(_.err.asInstanceOf[CompilationErrorDetails.RefinementFailure]))

    assert(
      result.isLeft
    )
  }

  pureTest("string field with length constraint - fail (dynamic)") {
    val dynamicStringSchema = dynamicSchemaFor[HasConstraintFields]

    val result = compile {
      WithSource.liftId(struct("minLength" -> "").mapK(WithSource.liftId))
    }(dynamicStringSchema)
      .leftMap(_.map(_.err.asInstanceOf[CompilationErrorDetails.RefinementFailure]))

    assert(
      result.isLeft
    )
  }

  pureTest("string - got int instead") {
    assert(
      compile {
        WithSource.liftId(42.mapK(WithSource.liftId))
      }(Schema.string) == Ior.left(
        NonEmptyChain.of(
          CompilationError.error(
            CompilationErrorDetails.TypeMismatch(
              NodeKind.StringLiteral,
              NodeKind.IntLiteral,
            ),
            SourceRange(Position(0), Position(0)),
          )
        )
      )
    )
  }

  pureTest("long") {
    assert(
      compile {
        WithSource.liftId(Long.MaxValue.mapK(WithSource.liftId))
      }(Schema.long) == Ior.right(Long.MaxValue)
    )
  }

  pureTest("long - out of range") {
    assert(
      compile {
        WithSource.liftId((BigInt(Long.MaxValue) + 1).mapK(WithSource.liftId))
      }(Schema.long).isLeft
    )
  }

  pureTest("int") {
    assert(
      compile {
        WithSource.liftId(42.mapK(WithSource.liftId))
      }(Schema.int) == Ior.right(42)
    )
  }

  pureTest("int - out of range") {
    assert(
      compile {
        WithSource.liftId((Int.MaxValue.toLong + 1L).mapK(WithSource.liftId))
      }(Schema.int).isLeft
    )
  }

  pureTest("short") {
    assert(
      compile {
        WithSource.liftId(42.mapK(WithSource.liftId))
      }(Schema.short) == Ior.right(42.toShort)
    )
  }

  pureTest("short - out of range") {
    assert(
      compile {
        WithSource.liftId((Short.MaxValue + 1).mapK(WithSource.liftId))
      }(Schema.short).isLeft
    )
  }

  pureTest("byte") {
    assert(
      compile {
        WithSource.liftId(Byte.MaxValue.mapK(WithSource.liftId))
      }(Schema.byte) == Ior.right(127.toByte)
    )
  }

  pureTest("byte - out of range") {
    assert(
      compile {
        WithSource.liftId((Byte.MaxValue + 1).mapK(WithSource.liftId))
      }(Schema.byte).isLeft
    )
  }

  pureTest("float") {
    assert(
      compile {
        WithSource.liftId(Float.MaxValue.mapK(WithSource.liftId))
      }(Schema.float) == Ior.right(Float.MaxValue)
    )
  }

  pureTest("float - out of range") {
    assert(
      compile {
        WithSource.liftId(Double.MaxValue.toString.mapK(WithSource.liftId))
      }(Schema.float).isLeft
    )
  }

  pureTest("double") {
    assert(
      compile {
        WithSource.liftId(Double.MaxValue.mapK(WithSource.liftId))
      }(Schema.double) == Ior.right(Double.MaxValue)
    )
  }

  pureTest("double - out of range") {
    assert(
      compile {
        WithSource.liftId((BigDecimal(Double.MaxValue) + 1).mapK(WithSource.liftId))
      }(Schema.double) == Ior.right(Double.MaxValue)
    )
  }

  test("bigint - OK") {
    forall { (bi: BigInt) =>
      assert(
        compile {
          WithSource.liftId(bi.mapK(WithSource.liftId))
        }(Schema.bigint) == Ior.right(bi)
      )
    }
  }

  pureTest("bigint - not accepting floats") {
    assert(
      compile {
        WithSource.liftId("40.50".mapK(WithSource.liftId))
      }(Schema.bigint).isLeft
    )
  }

  test("bigdecimal - OK") {
    forall { (bd: BigDecimal) =>
      assert(
        compile {
          WithSource.liftId(bd.mapK(WithSource.liftId))
        }(Schema.bigdecimal) == Ior.right(bd)
      )
    }
  }

  pureTest("bigdecimal - not a number") {
    assert(
      compile {
        WithSource.liftId("AAAA".mapK(WithSource.liftId))
      }(Schema.bigdecimal).isLeft
    )
  }

  pureTest("boolean") {
    assert(
      compile {
        WithSource.liftId(true.mapK(WithSource.liftId))
      }(Schema.boolean) == Ior.right(true)
    )
  }

  pureTest("null document") {
    assert(
      compile {
        WithSource.liftId(NullLiteral[WithSource]())
      }(Schema.document) == Ior.right(Document.nullDoc)
    )
  }

  pureTest("null doesn't work as anything like a string") {
    assert(
      compile {
        WithSource.liftId(NullLiteral[WithSource]())
      }(Schema.string).isLeft
    )
  }

  pureTest("blob") {
    assert(
      compile {
        WithSource.liftId("dGVzdA==".mapK(WithSource.liftId))
      }(Schema.bytes) == Ior.right(ByteArray("test".getBytes()))
    )
  }

  pureTest("blob - invalid") {
    assert(
      compile {
        WithSource.liftId("XYI519274n91lasdf/a'\'...,,".mapK(WithSource.liftId))
      }(Schema.bytes).isLeft
    )
  }

  pureTest("Simple struct") {
    assert(
      compile[Good] {
        WithSource.liftId {
          struct("howGood" -> 200).mapK(WithSource.liftId)
        }
      } == Ior.right(Good(200))
    )
  }

  pureTest("Using deprecated field in struct adds a warning with deprecation tags") {

    val result = compile[HasDeprecations](
      WithSource.liftId {
        struct(
          "hasMessage" -> true
        ).mapK(WithSource.liftId)
      }
    )
      .void
      .leftMap(_.filter(_.isWarning).map(err => (err.err, err.tags)))

    assert(
      result == Ior.left(
        Chain(
          (
            CompilationErrorDetails.DeprecatedItem(
              info = api.Deprecated(
                message = "Made-up reason".some,
                since = None,
              )
            ),
            Set(DiagnosticTag.Deprecated),
          )
        )
      )
    )
  }

  pureTest("struct with default field") {
    val result = compile[HasDefault](WithSource.liftId(struct().mapK(WithSource.liftId)))

    assert(result == Ior.right(HasDefault()))
  }

  pureTest("dynamic struct with default field") {
    val result =
      compile(WithSource.liftId(struct().mapK(WithSource.liftId)))(
        asDocument(dynamicSchemaFor[HasDefault])
      )

    // Object is empty here, but the server shall deserialize it providing the default
    assert(result == Ior.right(Document.obj()))
  }

  pureTest("Missing fields in struct") {
    assert(
      compile[Bad] {
        WithSource.liftId {
          struct().mapK(WithSource.liftId)
        }
      } == Ior.left(
        NonEmptyChain.of(
          CompilationError.error(
            CompilationErrorDetails
              .MissingField("evilName"),
            SourceRange(Position(0), Position(0)),
          ),
          CompilationError.error(
            CompilationErrorDetails.MissingField("powerLevel"),
            SourceRange(Position(0), Position(0)),
          ),
        )
      )
    )
  }

  pureTest("Missing fields in struct with mixins") {
    val result = compile[HasMixin] {
      WithSource.liftId {
        struct("name" -> "foo").mapK(WithSource.liftId)
      }
    }

    val expected = Ior.left(
      NonEmptyChain.of(
        CompilationError.error(
          CompilationErrorDetails.MissingField("id"),
          SourceRange(Position(0), Position(0)),
        )
      )
    )

    assert(result == expected)
  }

  pureTest("Missing fields in struct with mixins - dynamic") {
    val result =
      compile {
        WithSource.liftId {
          struct("name" -> "foo").mapK(WithSource.liftId)
        }
      }(dynamicSchemaFor[HasMixin])

    val expected = Ior.left(
      NonEmptyChain.of(
        CompilationError.error(
          CompilationErrorDetails.MissingField("id"),
          SourceRange(Position(0), Position(0)),
        )
      )
    )

    assert(result == expected)
  }

  pureTest("Missing fields in struct - 1 already present") {
    assert(
      compile[Bad] {
        WithSource.liftId {
          struct("evilName" -> "hello").mapK(WithSource.liftId)
        }
      } == Ior.left(
        NonEmptyChain.of(
          CompilationError.error(
            CompilationErrorDetails.MissingField("powerLevel"),
            SourceRange(Position(0), Position(0)),
          )
        )
      )
    )
  }
  pureTest("union") {
    assert(
      compile[Hero] {
        WithSource.liftId {
          struct(
            "good" -> struct("howGood" -> 200)
          ).mapK(WithSource.liftId)
        }
      } == Ior.right(Hero.GoodCase(Good(200)))
    )
  }

  pureTest("deprecated union member has warning but succeeds") {
    val result = compile[Hero] {
      WithSource.liftId {
        struct(
          "badder" -> struct("evilName" -> "Vader", "powerLevel" -> 9001)
        ).mapK(WithSource.liftId)
      }
    }

    val warning =
      CompilationError
        .warning(
          CompilationErrorDetails.DeprecatedItem(
            info = api.Deprecated(
              message = "No reason".some,
              since = "0.0.1".some,
            )
          ),
          SourceRange(Position(0), Position(0)),
        )
        .deprecated

    assert(
      result == Ior.bothNec(
        warning,
        Hero.BadderCase(Bad("Vader", 9001)),
      )
    )
  }

  pureTest("timestamp - OK") {
    val result =
      compile(WithSource.liftId("2022-07-11T17:42:28.000Z".mapK(WithSource.liftId)))(
        Schema.timestamp
      )
    val expected = Timestamp.parse("2022-07-11T17:42:28.000Z", TimestampFormat.DATE_TIME).get

    assert(
      result == Ior.right(expected)
    )
  }

  pureTest("timestamp - ignores format") {
    val result =
      compile(WithSource.liftId("2022-07-11T17:42:28.000Z".mapK(WithSource.liftId)))(
        Schema.timestamp.addHints(TimestampFormat.EPOCH_SECONDS: TimestampFormat)
      )

    val expected = Timestamp.parse("2022-07-11T17:42:28.000Z", TimestampFormat.DATE_TIME).get

    assert(
      result == Ior.right(expected)
    )
  }

  pureTest("timestamp - fails when the format is invalid") {
    val result = compile(WithSource.liftId("not-a-timestamp".mapK(WithSource.liftId)))(
      Schema.timestamp
    ).leftMap(_.map(_.err))

    assert(
      result == Ior.leftNec(
        CompilationErrorDetails.InvalidTimestampFormat(TimestampFormat.DATE_TIME)
      )
    )
  }

  pureTest("uuid - OK") {
    val result =
      compile(
        WithSource.liftId("9c8f8f8f-8f8f-8f8f-8f8f-8f8f8f8f8f8f".mapK(WithSource.liftId))
      )(
        Schema.uuid
      )
    assert(
      result == Ior.right(UUID.fromString("9c8f8f8f-8f8f-8f8f-8f8f-8f8f8f8f8f8f"))
    )
  }

  pureTest("enum - OK") {
    val result = compile[Power](WithSource.liftId("WIND".mapK(WithSource.liftId)))

    assert(
      result == Ior.right(Power.WIND)
    )
  }

  pureTest("enum - fallback to string value") {
    val aRange = SourceRange(Position(10), Position(20))

    val result = compile[Power](WithSource.liftId("Wind".mapK(WithSource.liftId)).withRange(aRange))

    val expected: QueryCompiler.Result[Power] = Ior.both(
      NonEmptyChain.one(
        CompilationError
          .warning(
            CompilationErrorDetails.EnumFallback("WIND"),
            aRange,
          )
          .deprecated
      ),
      Power.WIND,
    )

    assert(
      result == expected
    )
  }

  pureTest("enum - failure") {
    assert(
      compile[Power](WithSource.liftId("POISON".mapK(WithSource.liftId))) == Ior.left(
        NonEmptyChain.of(
          CompilationError.error(
            CompilationErrorDetails.UnknownEnumValue(
              "POISON",
              List("FIRE", "LIGHTNING", "WIND", "ICE"),
            ),
            SourceRange(Position(0), Position(0)),
          )
        )
      )
    )
  }

  pureTest("list of ints") {
    assert(
      compile[Ints](WithSource.liftId(List(1, 2, 3).mapK(WithSource.liftId))) == Ior.right(
        Ints(IndexedSeq(1, 2, 3))
      )
    )
  }

  pureTest("set of ints") {
    assert(
      compile[IntSet](WithSource.liftId(List(1, 2, 3).mapK(WithSource.liftId))) == Ior.right(
        IntSet(Set(1, 2, 3))
      )
    )
  }

  test("set of ints has warnings when duplicates are found") {
    forall { (range1: SourceRange, range2: SourceRange, range3: SourceRange) =>
      val actual = compile[IntSet](
        WithSource.liftId(
          Listed[WithSource](
            WithSource.liftId(
              List(
                WithSource.liftId(IntLiteral[WithSource]("1")).withRange(range1),
                WithSource.liftId(IntLiteral[WithSource]("2")).withRange(range2),
                WithSource.liftId(IntLiteral[WithSource]("2")).withRange(range3),
              )
            )
          )
        )
      )

      assert(
        actual == Ior.both(
          NonEmptyChain(
            CompilationError.warning(
              CompilationErrorDetails.DuplicateItem,
              range2,
            ),
            CompilationError.warning(
              CompilationErrorDetails.DuplicateItem,
              range3,
            ),
          ),
          Set(1, 2),
        )
      )
    }
  }

  test("set of ints has warnings when duplicates are found - dynamic") {
    forall { (range1: SourceRange, range2: SourceRange, range3: SourceRange) =>
      val actual =
        compile(
          WithSource.liftId(
            Listed[WithSource](
              WithSource.liftId(
                List(
                  WithSource.liftId(IntLiteral[WithSource]("1")).withRange(range1),
                  WithSource.liftId(IntLiteral[WithSource]("2")).withRange(range2),
                  WithSource.liftId(IntLiteral[WithSource]("2")).withRange(range3),
                )
              )
            )
          )
        )(asDocument(dynamicSchemaFor[IntSet]))

      assert(
        actual == Ior.both(
          NonEmptyChain(
            CompilationError.warning(
              CompilationErrorDetails.DuplicateItem,
              range2,
            ),
            CompilationError.warning(
              CompilationErrorDetails.DuplicateItem,
              range3,
            ),
          ),
          Document.array(Document.fromInt(1), Document.fromInt(2)),
        )
      )
    }
  }

  pureTest("set of struct fails when duplicates are found") {

    val item = struct("good" -> struct("howGood" -> 42))
    val compiledFailures = compile[FriendSet](
      WithSource.liftId(
        List(
          item,
          item,
        ).mapK(WithSource.liftId)
      )
    )
      .leftMap(_.map(_.err))

    assert(
      compiledFailures == Ior.both(
        NonEmptyChain(CompilationErrorDetails.DuplicateItem, CompilationErrorDetails.DuplicateItem),
        Set(Hero.GoodCase(Good(42))),
      )
    )
  }

  pureTest("set of struct fails when duplicates are found - dynamic") {

    val item = struct("good" -> struct("howGood" -> 42))
    val compiledFailures = compile(
      WithSource.liftId(
        List(
          item,
          item,
        ).mapK(WithSource.liftId)
      )
    )(asDocument(dynamicSchemaFor[FriendSet]))
      .leftMap(_.map(_.err))

    assert(
      compiledFailures == Ior.both(
        NonEmptyChain(CompilationErrorDetails.DuplicateItem, CompilationErrorDetails.DuplicateItem),
        Document.array(Document.obj("good" -> Document.obj("howGood" -> Document.fromInt(42)))),
      )
    )
  }

  pureTest("set of struct is OK when optional fields differ - dynamic") {
    val compiledDoc =
      compile(
        WithSource.liftId(
          List(
            struct("name" -> "Hello", "age" -> 42),
            struct("name" -> "Hello"),
          ).mapK(WithSource.liftId)
        )
      )(
        Schema.set(asDocument(dynamicSchemaFor[Person]))
      )

    val expected = Set(
      Document.obj(
        "name" -> Document.fromString("Hello"),
        "age" -> Document.fromInt(42),
      ),
      Document.obj(
        "name" -> Document.fromString("Hello")
      ),
    )

    assert(
      compiledDoc == Ior.right(
        expected
      )
    )
  }

  pureTest("list of strings where a list of ints is expected") {
    assert(
      compile[Ints](WithSource.liftId(List("hello", "world").mapK(WithSource.liftId))) == Ior.left(
        NonEmptyChain.of(
          CompilationError.error(
            CompilationErrorDetails.TypeMismatch(NodeKind.IntLiteral, NodeKind.StringLiteral),
            SourceRange(Position(0), Position(0)),
          ),
          CompilationError.error(
            CompilationErrorDetails.TypeMismatch(NodeKind.IntLiteral, NodeKind.StringLiteral),
            SourceRange(Position(0), Position(0)),
          ),
        )
      )
    )
  }

  implicit val arbInputNode: Arbitrary[InputNode[WithSource]] = Arbitrary(genInputNode(2))
  implicit val showWast: Show[QueryCompiler.WAST] = Show.fromToString

  test("anything to document matches") {
    forall((wast: QueryCompiler.WAST) =>
      assert(
        compile[Document](wast)(Schema.document).isRight
      )
    )
  }

  pureTest("list of structs to document") {
    assert(
      compile(
        WithSource.liftId(
          List(
            struct("good" -> true, "howGood" -> 200),
            struct("name" -> "aaa"),
          ).mapK(WithSource.liftId)
        )
      )(Schema.document) == Ior.right(
        Document.array(
          Document.obj(
            "good" -> Document.fromBoolean(true),
            "howGood" -> Document.fromInt(200),
          ),
          Document.obj(
            "name" -> Document.fromString("aaa")
          ),
        )
      )
    )
  }

  pureTest("timestamp matches instant") {
    val s = "2022-07-11T17:42:28.000Z"
    val ts = time.Instant.parse(s)

    val result = compile[MyInstant](WithSource.liftId(s.mapK(WithSource.liftId)))

    assert(result == Ior.right(ts))
  }

  private def parseAndCompile[Alg[_[_, _, _, _, _]]](
    service: Service[Alg]
  )(
    q: String
  ) = playground
    .OperationCompiler
    .fromService(service)
    .compile(
      SourceParser[Query].parse(q).toTry.get
    )

  pureTest("deprecated service's use clause") {
    parseAndCompile(DeprecatedServiceGen)(
      """use service demo.smithy#LiterallyAnyService
        |hello {}""".stripMargin
    ).left match {
      case Some(cf: CompilationFailed) =>
        val result = cf
          .errors
          .filter(_.isWarning)
          .map(e => (e.err, e.range, e.tags))

        val expected = List(
          (
            CompilationErrorDetails.DeprecatedItem(
              api.Deprecated(Some("don't use"), Some("0.0.0"))
            ),
            SourceRange(
              Position("use service ".length),
              Position("use service demo.smithy#LiterallyAnyService".length),
            ),
            Set(DiagnosticTag.Deprecated),
          )
        )

        assert(result == expected)

      case e => failure("Unexpected exception: " + e)
    }
  }

  pureTest("deprecated operation") {
    parseAndCompile(DeprecatedServiceGen)(
      """DeprecatedOperation { a = 42 }""".stripMargin
    ).left match {
      case Some(cf: CompilationFailed) =>
        val result = cf
          .errors
          .filter(_.isWarning)
          .map(e => (e.err, e.range, e.tags))

        val expected = List(
          (
            CompilationErrorDetails.DeprecatedItem(
              api.Deprecated(Some("don't use"), Some("0.0.0"))
            ),
            SourceRange(
              Position(0),
              Position("DeprecatedOperation".length),
            ),
            Set(DiagnosticTag.Deprecated),
          )
        )

        assert(result == expected)

      case e => failure("Unexpected exception: " + e)
    }
  }

  pureTest("deprecated operation - only warnings") {
    parseAndCompile(DeprecatedServiceGen)(
      "use service demo.smithy#DeprecatedService\nDeprecatedOperation { }"
    ).left match {
      case Some(cf: CompilationFailed) =>
        val result = cf
          .errors
          .filter(_.isWarning)
          .map(e => (e.err, e.range, e.tags))

        val expected = List(
          (
            CompilationErrorDetails.DeprecatedItem(
              api.Deprecated(Some("don't use"), Some("0.0.0"))
            ),
            SourceRange(
              Position("use service demo.smithy#DeprecatedService\n".length),
              Position("use service demo.smithy#DeprecatedService\nDeprecatedOperation".length),
            ),
            Set(DiagnosticTag.Deprecated),
          ),
          (
            CompilationErrorDetails.DeprecatedItem(
              api.Deprecated(Some("don't use"), Some("0.0.0"))
            ),
            SourceRange(
              Position("use service ".length),
              Position("use service demo.smithy#DeprecatedService".length),
            ),
            Set(DiagnosticTag.Deprecated),
          ),
        )

        assert(result == expected)

      case e => failure("Unexpected exception: " + e)
    }
  }
}
