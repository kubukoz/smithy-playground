package playground

import cats.Apply
import cats.Id
import cats.data.Ior
import cats.data.IorNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.implicits._
import playground.CompilationErrorDetails._
import playground.smithyql._
import playground.smithyutil._
import smithy.api
import smithy.api.TimestampFormat
import smithy4s.Bijection
import smithy4s.ByteArray
import smithy4s.Document
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.CollectionTag.IndexedSeqTag
import smithy4s.schema.CollectionTag.ListTag
import smithy4s.schema.CollectionTag.SetTag
import smithy4s.schema.CollectionTag.VectorTag
import smithy4s.schema.EnumValue
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
import smithy4s.schema.Primitive.PUnit
import smithy4s.schema.Schema
import smithy4s.schema.SchemaField
import smithy4s.schema.SchemaVisitor

import java.util.Base64
import java.util.UUID

import types._
import util.chaining._
import PartialCompiler.WAST

trait PartialCompiler[A] {
  final def emap[B](f: A => PartialCompiler.Result[B]): PartialCompiler[B] =
    ast => compile(ast).flatMap(f)

  def compile(ast: WAST): PartialCompiler.Result[A]

}

object PartialCompiler {
  type Result[+A] = IorNec[CompilationError, A]

  implicit val apply: Apply[PartialCompiler] =
    new Apply[PartialCompiler] {
      def map[A, B](fa: PartialCompiler[A])(f: A => B): PartialCompiler[B] = fa.compile(_).map(f)

      def ap[A, B](ff: PartialCompiler[A => B])(fa: PartialCompiler[A]): PartialCompiler[B] =
        wast => (ff.compile(wast), fa.compile(wast)).parMapN((a, b) => a(b))
    }

  type WAST = WithSource[InputNode[WithSource]]

  val pos: PartialCompiler[SourceRange] = _.range.rightIor
  val unit: PartialCompiler[Unit] = _ => ().rightIor

  def typeCheck[A](
    expected: NodeKind
  )(
    f: PartialFunction[InputNode[WithSource], A]
  ): PartialCompiler[WithSource[A]] =
    ast =>
      ast
        .traverse(f.lift)
        .toRightIor(
          NonEmptyChain(
            CompilationError.error(
              TypeMismatch(
                expected,
                ast.value.kind,
              ),
              ast.range,
            )
          )
        )

}

object QueryCompiler {
  val full = new TransitiveCompiler(AddDynamicRefinements) andThen QueryCompilerInternal

}

object QueryCompilerInternal extends SchemaVisitor[PartialCompiler] {

  private def checkRange[A, B](
    pc: PartialCompiler[A]
  )(
    tag: String
  )(
    matchToRange: A => Option[B]
  ) = (pc, PartialCompiler.pos).tupled.emap { case (i, range) =>
    matchToRange(i)
      .toRightIor(
        CompilationError
          .error(NumberOutOfRange(i.toString, tag), range)
      )
      .toIorNec
  }

  def primitive[P](shapeId: ShapeId, hints: Hints, tag: Primitive[P]): PartialCompiler[P] =
    tag match {
      case PString => string
      case PBoolean =>
        PartialCompiler
          .typeCheck(NodeKind.Bool) { case b @ BooleanLiteral(_) => b }
          .map(_.value.value)
      case PUnit     => struct(shapeId, hints, Vector.empty, _ => ())
      case PLong     => checkRange(integer)("int")(_.toLongOption)
      case PInt      => checkRange(integer)("int")(_.toIntOption)
      case PShort    => checkRange(integer)("short")(_.toShortOption)
      case PByte     => checkRange(integer)("byte")(_.toByteOption)
      case PFloat    => checkRange(integer)("float")(_.toFloatOption)
      case PDouble   => checkRange(integer)("double")(_.toDoubleOption)
      case PDocument => document
      case PBlob =>
        (string, PartialCompiler.pos).tupled.emap { case (s, range) =>
          Either
            .catchNonFatal(Base64.getDecoder().decode(s))
            .map(ByteArray(_))
            .leftMap(_ => CompilationError.error(CompilationErrorDetails.InvalidBlob, range))
            .toIor
            .toIorNec
        }
      case PBigDecimal =>
        checkRange(integer)("bigdecimal") { s =>
          Either.catchNonFatal(BigDecimal(s)).toOption
        }
      case PBigInt =>
        checkRange(integer)("bigint") { s =>
          Either.catchNonFatal(BigInt(s)).toOption
        }
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

  private val integer: PartialCompiler[String] = PartialCompiler
    .typeCheck(NodeKind.IntLiteral) { case i @ IntLiteral(_) => i }
    .map(_.value.value)

  def collection[C[_], A](
    shapeId: ShapeId,
    hints: Hints,
    tag: CollectionTag[C],
    member: Schema[A],
  ): PartialCompiler[C[A]] =
    tag match {
      case SetTag =>
        val memberToDoc = Document.Encoder.fromSchema(member)

        listWithPos(member.compile(this)).emap { items =>
          val success = items.map(_._1).toSet

          val duplications = items
            .groupBy { case (v, _) => memberToDoc.encode(v) }
            .map(_._2)
            .filter(_.sizeIs > 1)
            .flatMap(_.map(_._2))
            // todo: reorganize this so it only shows the warning once with extra locations (which ideally would be marked as unused, but idk if possible)
            .map(CompilationError.warning(CompilationErrorDetails.DuplicateItem, _))
            .toList
            .pipe(NonEmptyChain.fromSeq(_))

          duplications match {
            case None         => success.rightIor
            case Some(errors) => Ior.both(errors, success)
          }
        }

      case ListTag       => listOf(member)
      case IndexedSeqTag => listOf(member).map(_.toIndexedSeq)
      case VectorTag     => listOf(member).map(_.toVector)
    }

  private def listOf[A](member: Schema[A]) = listWithPos(member.compile(this)).map(_.map(_._1))

  def map[K, V](
    shapeId: ShapeId,
    hints: Hints,
    key: Schema[K],
    value: Schema[V],
  ): PartialCompiler[Map[K, V]] = {
    val fk = key.compile(this)
    val fv = value.compile(this)

    PartialCompiler
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap { struct =>
        val fields = struct.value.fields.value.value

        fields
          .parTraverse { case (k, v) =>
            (
              fk.compile(k.map { key =>
                StringLiteral[Id](key.text).mapK(WithSource.liftId)
              }),
              fv.compile(v),
            ).parTupled
          }
          .map(_.toMap)
      }
  }

  def struct[S](
    shapeId: ShapeId,
    hints: Hints,
    fieldsRaw: Vector[SchemaField[S, _]],
    make: IndexedSeq[Any] => S,
  ): PartialCompiler[S] = {
    val fields = fieldsRaw.map(_.mapK(this))

    val validFields = fields.map(_.label)
    val deprecatedFields =
      fieldsRaw.flatMap { f =>
        f.hints.get(api.Deprecated).tupleLeft(f.label)
      }.toMap

    PartialCompiler
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap { struct =>
        // this is a list to keep the original type's ordering
        val remainingValidFields =
          validFields
            .filterNot(
              struct.value.fields.value.keys.map(_.value.text).toSet
            )
            .toList

        val presentKeys = struct.value.fields.value.keys

        val extraFieldErrors: PartialCompiler.Result[Unit] = presentKeys
          .filterNot(field => validFields.contains(field.value.text))
          .map { unexpectedKey =>
            CompilationError.error(
              UnexpectedField(remainingValidFields),
              unexpectedKey.range,
            )
          }
          .toList
          .toNel
          .map(NonEmptyChain.fromNonEmptyList)
          .toBothLeft(())
          .combine(Ior.right(()))

        val deprecatedFieldWarnings: PartialCompiler.Result[Unit] = presentKeys
          .flatMap { key =>
            deprecatedFields.get(key.value.text).map { info =>
              CompilationError.deprecation(info, key.range)
            }
          }
          .toList
          .toNel
          .map(NonEmptyChain.fromNonEmptyList)
          .toBothLeft(())
          .combine(Ior.right(()))

        val buildStruct = fields
          .parTraverse { field =>
            val fieldOpt = struct
              .value
              .fields
              .value
              .byName(field.label)(_.value)
              .parTraverse(field.instance.compile)

            if (field.isOptional)
              fieldOpt
            else
              fieldOpt.flatMap {
                _.toRightIor(
                  CompilationError.error(
                    MissingField(field.label),
                    struct.value.fields.range,
                  )
                ).toIorNec
              }
          }

        buildStruct.map(make) <& extraFieldErrors <& deprecatedFieldWarnings
      }

  }

  def union[U](
    shapeId: ShapeId,
    hints: Hints,
    alternatives: Vector[Alt[Schema, U, _]],
    dispatcher: Alt.Dispatcher[Schema, U],
  ): PartialCompiler[U] = {
    val alternativesCompiled = alternatives.map(_.mapK(this)).groupBy(_.label).map(_.map(_.head))
    val deprecatedAlternativeLabels =
      alternatives.flatMap(alt => alt.hints.get(api.Deprecated).tupleLeft(alt.label)).toMap

    val labels = NonEmptyList.fromListUnsafe(alternatives.toList).map(_.label)

    PartialCompiler
      // todo: should say it's a union
      .typeCheck(NodeKind.Struct) { case s @ Struct(_) => s }
      .emap {
        case s if s.value.fields.value.size == 1 =>
          val defs = s.value.fields.value
          def go[A](
            alt: Alt[PartialCompiler, U, A]
          ): PartialCompiler[U] = alt.instance.map(alt.inject)

          val (k, v) = defs.head
          val op =
            alternativesCompiled
              .get(k.value.text)
              .toRightIor(
                CompilationError.error(
                  MissingDiscriminator(labels),
                  s.range,
                )
              )
              .toIorNec

          val deprecationWarning: PartialCompiler.Result[Unit] = deprecatedAlternativeLabels
            .get(k.value.text)
            .map { info =>
              CompilationError
                .deprecation(info, k.range)
            }
            .toList
            .toNel
            .map(NonEmptyChain.fromNonEmptyList)
            .toBothLeft(())
            .combine(Ior.right(()))

          op.flatMap(go(_).compile(v)) <& deprecationWarning

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
  ): PartialCompiler[B] = schema.compile(this).map(bijection.apply)

  def refine[A, B](
    schema: Schema[A],
    refinement: Refinement[A, B],
  ): PartialCompiler[B] = surject(schema.compile(this), refinement)

  private def surject[A, B](
    pc: PartialCompiler[A],
    refinement: Refinement[A, B],
  ): PartialCompiler[B] = (pc, PartialCompiler.pos).tupled.emap { case (a, pos) =>
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

  def lazily[A](suspend: Lazy[Schema[A]]): PartialCompiler[A] = {
    val it = suspend.map(_.compile(this))

    it.value.compile(_)
  }

  val stringLiteral =
    PartialCompiler.typeCheck(NodeKind.StringLiteral) { case StringLiteral(s) => s }

  val document: PartialCompiler[Document] =
    _.value match {
      case BooleanLiteral(value) => Document.fromBoolean(value).pure[PartialCompiler.Result]
      case IntLiteral(value) =>
        Document.fromBigDecimal(BigDecimal(value)).pure[PartialCompiler.Result]
      case StringLiteral(value) => Document.fromString(value).pure[PartialCompiler.Result]
      // parTraverse in this file isn't going to work like you think it will
      case Listed(values) => values.value.parTraverse(document.compile(_)).map(Document.array(_))
      case Struct(fields) =>
        fields
          .value
          .value
          .parTraverse { case (key, value) => document.compile(value).tupleLeft(key.value.text) }
          .map(Document.obj(_: _*))
      case NullLiteral() => Document.nullDoc.rightIor
    }

  val string = stringLiteral.map(_.value)

  def enumeration[E](
    shapeId: ShapeId,
    hints: Hints,
    values: List[EnumValue[E]],
    total: E => EnumValue[E],
  ): PartialCompiler[E] = (string, PartialCompiler.pos).tupled.emap { case (name, range) =>
    values
      .foreach(v =>
        System
          .err
          .println(
            s"""EnumValue(stringValue = ${v.stringValue}, intValue = ${v.intValue}, value = ${v.value}, name = ${v.name}, hints = ${v.hints})"""
          )
      )
    val byValue = values
      .find(_.stringValue == name)

    val byName = values
      .find(_.name == name)

    (byName, byValue) match {
      case (Some(v), _) => v.value.pure[PartialCompiler.Result]

      case (None, Some(v)) =>
        Ior.bothNec(CompilationError.warning(EnumFallback(v.name), range).deprecated, v.value)

      case (None, None) =>
        Ior.leftNec(CompilationError.error(UnknownEnumValue(name, values.map(_.name)), range))
    }

  }

  private def listWithPos[S](
    fs: PartialCompiler[S]
  ): PartialCompiler[List[(S, SourceRange)]] = PartialCompiler
    .typeCheck(NodeKind.Listed) { case l @ Listed(_) => l }
    .emap(
      _.value
        .values
        .value
        .parTraverse { item =>
          (fs, PartialCompiler.pos).tupled.compile(item)
        }
    )

}
