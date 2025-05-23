package playground

import cats.Id
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import playground.CompilationErrorDetails.*
import playground.smithyql.*
import playground.smithyutil.*
import smithy.api
import smithy.api.TimestampFormat
import smithy4s.Bijection
import smithy4s.Blob
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.EnumTag
import smithy4s.schema.EnumValue
import smithy4s.schema.Field
import smithy4s.schema.Primitive
import smithy4s.schema.Primitive.PBigDecimal
import smithy4s.schema.Primitive.PBigInt
import smithy4s.schema.Primitive.PBlob
import smithy4s.schema.Primitive.PBoolean
import smithy4s.schema.Primitive.PByte
import smithy4s.schema.Primitive.PDocument
import smithy4s.schema.Primitive.PDouble
import smithy4s.schema.Primitive.PFloat
import smithy4s.schema.Primitive.PInt
import smithy4s.schema.Primitive.PLong
import smithy4s.schema.Primitive.PShort
import smithy4s.schema.Primitive.PString
import smithy4s.schema.Primitive.PTimestamp
import smithy4s.schema.Primitive.PUUID
import smithy4s.schema.Schema
import smithy4s.schema.SchemaVisitor
import smithy4s.~>
import types.*
import util.chaining.*

import java.util.Base64
import java.util.UUID
import scala.collection.immutable.ListMap

object QueryCompilerVisitor {
  val full: Schema ~> QueryCompiler =
    Schema.transformTransitivelyK(AddDynamicRefinements) andThen QueryCompilerVisitorInternal
}

object QueryCompilerVisitorInternal extends SchemaVisitor[QueryCompiler] {

  private def checkRange[B](
    pc: QueryCompiler[BigDecimal]
  )(
    tag: String
  )(
    matchToRange: PartialFunction[BigDecimal, B]
  ) = (pc, QueryCompiler.pos).tupled.emap { case (i, range) =>
    Either
      .catchOnly[ArithmeticException](matchToRange.lift(i).toRight(()))
      .leftWiden[Any]
      .flatten
      .leftMap(_ =>
        CompilationError
          .error(NumberOutOfRange(i.toString, tag), range)
      )
      .toIor
      .toIorNec
  }

  def primitive[P](
    shapeId: ShapeId,
    hints: Hints,
    tag: Primitive[P],
  ): QueryCompiler[P] =
    tag match {
      case PString => string
      case PBoolean =>
        QueryCompiler
          .typeCheck(NodeKind.Bool) { case b @ BooleanLiteral(_) => b }
          .map(_.value.value)
      case PLong     => checkRange(number)("int")(_.toLongExact)
      case PInt      => checkRange(number)("int")(_.toIntExact)
      case PShort    => checkRange(number)("short")(_.toShortExact)
      case PByte     => checkRange(number)("byte")(_.toByteExact)
      case PFloat    => checkRange(number)("float") { case i if i.isDecimalFloat => i.toFloat }
      case PDouble   => checkRange(number)("double") { case i if i.isDecimalDouble => i.toDouble }
      case PDocument => document
      case PBlob =>
        (string, QueryCompiler.pos).tupled.emap { case (s, range) =>
          Either
            .catchNonFatal(Base64.getDecoder().decode(s))
            .map(Blob(_))
            .leftMap(_ => CompilationError.error(CompilationErrorDetails.InvalidBlob, range))
            .toIor
            .toIorNec
        }
      case PBigDecimal => number
      case PBigInt     => checkRange(number)("bigint")(_.toBigIntExact.get)
      case PUUID =>
        stringLiteral.emap { s =>
          Either
            .catchOnly[IllegalArgumentException](UUID.fromString(s.value))
            .toIor
            .leftMap(_ => CompilationError.error(InvalidUUID, s.range))
            .toIorNec
        }

      case PTimestamp =>
        stringLiteral.emap { s =>
          // We don't support other formats for the simple reason that it's not necessary:
          // this is just like multiple union encodings - it doesn't matter how you write your queries,
          // the actual serialization format will be used by the client when we eventually use it in the Runner.
          val format = TimestampFormat.DATE_TIME

          Timestamp
            .parse(s.value, format)
            .toRightIor(
              CompilationError.error(
                InvalidTimestampFormat(format),
                s.range,
              )
            )
            .toIorNec
        }
    }

  private val number: QueryCompiler[BigDecimal] = QueryCompiler
    .typeCheck(NodeKind.IntLiteral) { case i @ IntLiteral(_) => i }
    .map(_.value.value)
    .map(BigDecimal(_))

  def collection[C[_], A](
    shapeId: ShapeId,
    hints: Hints,
    tag: CollectionTag[C],
    member: Schema[A],
  ): QueryCompiler[C[A]] = {
    val base =
      if (hints.has(api.UniqueItems))
        uniqueListOf(member)
      else
        listOf(member)

    base
      .map(_.iterator)
      .map(tag.fromIterator(_))
  }

  private def uniqueListOf[A](
    member: Schema[A]
  ): QueryCompiler[List[A]] = {
    val memberToDoc = Document.Encoder.fromSchema(member)

    listWithPos(member.compile(this)).emap { items =>
      val itemsGrouped = items
        .groupBy { case (v, _) => memberToDoc.encode(v) }

      val duplications = itemsGrouped
        .values
        .filter(_.sizeIs > 1)
        .flatMap(_.map(_._2))
        // nice to have: reorganize this so it only shows the warning once with extra locations (which ideally would be marked as unused, but idk if possible)
        .map(CompilationError.warning(CompilationErrorDetails.DuplicateItem, _))
        .toList
        .pipe(NonEmptyChain.fromSeq(_))

      val success = itemsGrouped.map(_._2.head._1).toList

      duplications match {
        case None         => success.rightIor
        case Some(errors) => Ior.both(errors, success)
      }
    }
  }

  private def listOf[A](
    member: Schema[A]
  ) = listWithPos(member.compile(this)).map(_.map(_._1))

  def map[K, V](
    shapeId: ShapeId,
    hints: Hints,
    key: Schema[K],
    value: Schema[V],
  ): QueryCompiler[Map[K, V]] = {
    val fk = key.compile(this)
    val fv = value.compile(this)

    QueryCompiler
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap { struct =>
        val fields = struct.value.fields.value.value

        fields
          .parTraverse { binding =>
            (
              fk.compile(binding.identifier.map { key =>
                StringLiteral[Id](key.text).mapK(WithSource.liftId)
              }),
              fv.compile(binding.value),
            ).parTupled
          }
          .map(_.toMap)
      }
  }

  private trait FieldCompiler[A] {
    def compiler: QueryCompiler[A]
    def default: Option[A]
  }

  private object FieldCompiler {

    def compile[A](
      field: Field[?, A]
    ): FieldCompiler[A] =
      new FieldCompiler[A] {
        override val compiler: QueryCompiler[A] = field.schema.compile(QueryCompilerVisitorInternal)

        override val default: Option[A] = field.schema.getDefaultValue

      }

  }

  def struct[S](
    shapeId: ShapeId,
    hints: Hints,
    fieldsRaw: Vector[Field[S, ?]],
    make: IndexedSeq[Any] => S,
  ): QueryCompiler[S] = {
    val fields = fieldsRaw
      .map(f => f.label -> FieldCompiler.compile(f))
      .to(ListMap)

    val validFields = fields
    val deprecatedFields =
      fieldsRaw.flatMap { f =>
        f.hints.get(api.Deprecated).tupleLeft(f.label)
      }.toMap

    QueryCompiler
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap { struct =>
        val presentKeys = struct.value.fields.value.keys

        // this is a list to keep the original type's ordering
        val remainingValidFields = validFields -- presentKeys.map(_.value.text).toSet

        val extraFieldErrors: QueryCompiler.Result[Unit] = presentKeys
          .filterNot(field => validFields.keySet.contains_(field.value.text))
          .map { unexpectedKey =>
            CompilationError.error(
              UnexpectedField(remainingValidFields.keys.toList),
              unexpectedKey.range,
            )
          }
          .toNel
          .map(NonEmptyChain.fromNonEmptyList)
          .toBothLeft(())
          .combine(Ior.right(()))

        val deprecatedFieldWarnings: QueryCompiler.Result[Unit] = presentKeys
          .flatMap { key =>
            deprecatedFields.get(key.value.text).map { info =>
              CompilationError.deprecation(DeprecatedInfo.fromHint(info), key.range)
            }
          }
          .toList
          .toNel
          .map(NonEmptyChain.fromNonEmptyList)
          .toBothLeft(())
          .combine(Ior.right(()))

        def handleField[T](
          label: String,
          field: FieldCompiler[T],
        ): QueryCompiler.Result[T] = {
          val fieldByName =
            struct
              .value
              .fields
              .value
              .byName(label)(_.value)

          // Note: defaults get no special handling in dynamic schemas (in which a field with a default is considered optional).
          // There's no real need to provide the default value in a dynamic client, as it can just omit the field in the request being sent.
          // The server shall provide the default value on its own.
          // This `orElse` fallback will arguably never be hit in practice, but it's here for completeness - just in case the compiler ends up being used with static
          fieldByName
            .parTraverse(field.compiler.compile)
            .map(_.orElse(field.default))
            .flatMap {
              _.toRightIor(
                CompilationError.error(
                  MissingField(label),
                  struct.value.fields.range,
                )
              ).toIorNec
            }
        }

        val buildStruct = fields
          .toVector
          .parTraverse { case (label, instance) => handleField(label, instance) }

        buildStruct.map(make) <& extraFieldErrors <& deprecatedFieldWarnings
      }
  }

  def union[U](
    shapeId: ShapeId,
    hints: Hints,
    alternatives: Vector[Alt[U, ?]],
    dispatcher: Alt.Dispatcher[U],
  ): QueryCompiler[U] = {
    def handleAlt[A](
      alt: Alt[U, A]
    ): QueryCompiler[U] = alt
      .schema
      .compile(QueryCompilerVisitorInternal.this)
      .map(alt.inject)

    val alternativesCompiled = alternatives
      .groupByNev(_.label)
      .fmap(_.head)
      .fmap(handleAlt(_))

    val deprecatedAlternativeLabels =
      alternatives.flatMap(alt => alt.hints.get(api.Deprecated).tupleLeft(alt.label)).toMap

    val labels = NonEmptyList.fromListUnsafe(alternatives.toList).map(_.label)

    QueryCompiler
      // todo: should say it's a union
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap {
        case s if s.value.fields.value.size === 1 =>
          val definition = s.value.fields.value.head
          val key = definition.identifier

          val op =
            alternativesCompiled
              .get(key.value.text)
              .toRightIor(
                CompilationError.error(
                  MissingDiscriminator(labels),
                  s.range,
                )
              )
              .toIorNec

          val deprecationWarning: QueryCompiler.Result[Unit] = deprecatedAlternativeLabels
            .get(key.value.text)
            .map { info =>
              CompilationError
                .deprecation(DeprecatedInfo.fromHint(info), key.range)
            }
            .toList
            .toNel
            .map(NonEmptyChain.fromNonEmptyList)
            .toBothLeft(())
            .combine(Ior.right(()))

          op.flatMap(_.compile(definition.value)) <& deprecationWarning

        case s if s.value.fields.value.isEmpty =>
          CompilationError
            .error(
              EmptyStruct(labels),
              s.range,
            )
            .leftIor
            .toIorNec

        case s =>
          CompilationError
            .error(
              StructMismatch(
                s.value.fields.value.keys.map(_.value.text),
                labels,
              ),
              s.range,
            )
            .leftIor
            .toIorNec
      }
  }

  def biject[A, B](
    schema: Schema[A],
    bijection: Bijection[A, B],
  ): QueryCompiler[B] = schema.compile(this).map(bijection.apply)

  def refine[A, B](
    schema: Schema[A],
    refinement: Refinement[A, B],
  ): QueryCompiler[B] = surject(schema.compile(this), refinement)

  private def surject[A, B](
    pc: QueryCompiler[A],
    refinement: Refinement[A, B],
  ): QueryCompiler[B] = (pc, QueryCompiler.pos).tupled.emap { case (a, pos) =>
    refinement(a)
      .toIor
      .leftMap { msg =>
        CompilationError.error(
          CompilationErrorDetails.RefinementFailure(msg),
          pos,
        )
      }
      .toIorNec
  }

  def lazily[A](
    suspend: Lazy[Schema[A]]
  ): QueryCompiler[A] = {
    val it = suspend.map(_.compile(this))

    it.value.compile(_)
  }

  val stringLiteral: QueryCompiler[WithSource[String]] =
    QueryCompiler.typeCheck(NodeKind.StringLiteral) { case StringLiteral(s) => s }

  val document: QueryCompiler[Document] =
    _.value match {
      case BooleanLiteral(value) => Document.fromBoolean(value).pure[QueryCompiler.Result]
      case IntLiteral(value) =>
        Document.fromBigDecimal(BigDecimal(value)).pure[QueryCompiler.Result]
      case StringLiteral(value) => Document.fromString(value).pure[QueryCompiler.Result]
      // parTraverse in this file isn't going to work like you think it will
      case Listed(values) => values.value.parTraverse(document.compile(_)).map(Document.array(_))
      case Struct(fields) =>
        fields
          .value
          .value
          .parTraverse { binding =>
            document.compile(binding.value).tupleLeft(binding.identifier.value.text)
          }
          .map(Document.obj(_*))
      case NullLiteral() => Document.nullDoc.rightIor
    }

  val string: QueryCompiler[String] = stringLiteral.map(_.value)

  def option[A](
    schema: Schema[A]
  ): QueryCompiler[Option[A]] = {
    val underlying = schema.compile(this)

    in =>
      in.value match {
        case NullLiteral() => None.rightIor
        case _             => underlying.compile(in).map(Some(_))
      }
  }

  override def enumeration[E](
    shapeId: ShapeId,
    hints: Hints,
    tag: EnumTag[E],
    values: List[EnumValue[E]],
    total: E => EnumValue[E],
  ): QueryCompiler[E] = (string, QueryCompiler.pos).tupled.emap { case (name, range) =>
    val byValue = values
      .find(_.stringValue === name)

    val byName = values
      .find(_.name === name)

    (byName, byValue) match {
      case (Some(v), _) => v.value.pure[QueryCompiler.Result]

      case (None, Some(v)) =>
        Ior.bothNec(CompilationError.warning(EnumFallback(v.name), range).deprecated, v.value)

      case (None, None) =>
        Ior.leftNec(CompilationError.error(UnknownEnumValue(name, values.map(_.name)), range))
    }

  }

  private def listWithPos[S](
    fs: QueryCompiler[S]
  ): QueryCompiler[List[
    (
      S,
      SourceRange,
    )
  ]] = QueryCompiler
    .typeCheck(NodeKind.Listed) { case l @ Listed(_) => l }
    .emap(
      _.value
        .values
        .value
        .parTraverse { item =>
          (fs, QueryCompiler.pos).tupled.compile(item)
        }
    )

}
