package playground.smithyql

import cats.data.NonEmptyList
import cats.implicits._
import org.typelevel.paiges.Doc
import playground.TextUtils
import playground.smithyql.CompletionItem.InsertUseClause.NotRequired
import playground.smithyql.CompletionItem.InsertUseClause.Required
import smithy4s.Endpoint
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.EnumValue
import smithy4s.schema.Field
import smithy4s.schema.Primitive
import smithy4s.schema.Schema
import smithy4s.schema.Schema.BijectionSchema
import smithy4s.schema.Schema.EnumerationSchema
import smithy4s.schema.Schema.LazySchema
import smithy4s.schema.Schema.MapSchema
import smithy4s.schema.Schema.PrimitiveSchema
import smithy4s.schema.Schema.StructSchema
import smithy4s.schema.Schema.UnionSchema
import smithy4s.schema.SchemaAlt
import smithy4s.schema.SchemaField
import smithy4s.schema.SchemaVisitor

import WithSource.NodeContext.PathEntry
import java.util.UUID
import smithy.api

trait CompletionResolver[+A] {
  def getCompletions(ctx: List[PathEntry]): List[CompletionItem]
  def retag[B]: CompletionResolver[B] = getCompletions(_)
}

final case class CompletionItem(
  kind: CompletionItemKind,
  label: String,
  insertText: InsertText,
  detail: String,
  description: Option[String],
  deprecated: Boolean,
  docs: Option[String],
  extraTextEdits: List[TextEdit],
)

sealed trait TextEdit extends Product with Serializable

object TextEdit {
  final case class Insert(text: String, pos: Position) extends TextEdit
}

sealed trait InsertText extends Product with Serializable

object InsertText {
  final case class JustString(value: String) extends InsertText
  final case class SnippetString(value: String) extends InsertText
}

object CompletionItem {

  def fromField(
    field: Field[CompletionResolver, _, _],
    schema: Schema[_],
  ): CompletionItem = fromHints(
    kind = CompletionItemKind.Field,
    label = field.label,
    insertText = InsertText.JustString(s"${field.label} = "),
    schema = schema,
  )

  def fromAlt(
    alt: Alt[CompletionResolver, _, _],
    schema: Schema[_],
  ): CompletionItem = fromHints(
    kind = CompletionItemKind.UnionMember,
    label = alt.label,
    // todo: unions aren't only for structs: this makes an invalid assumption
    // by inserting {} at all times
    insertText = InsertText.SnippetString(s"${alt.label} = {$$0},"),
    schema = schema,
  )

  def fromHints(
    kind: CompletionItemKind,
    label: String,
    insertText: InsertText,
    schema: Schema[_],
  ): CompletionItem = {
    val isField = kind == CompletionItemKind.Field

    CompletionItem(
      kind = kind,
      label = label,
      insertText = insertText,
      deprecated = schema.hints.get(smithy.api.Deprecated).isDefined,
      detail = describeType(
        isField = isField,
        schema = schema,
      ),
      description = schema.shapeId.namespace.some,
      docs = buildDocumentation(
        schema.hints,
        isField,
      ),
      extraTextEdits = Nil,
    )
  }

  def describeType(isField: Boolean, schema: Schema[_]): String = {
    val isOptional = isField && schema.hints.get(smithy.api.Required).isEmpty

    val optionalPrefix =
      if (isOptional)
        "?"
      else
        ""
    show"$optionalPrefix: ${describeSchema(schema)()}"
  }

  private val describePrimitive: Primitive[_] => String = {
    import smithy4s.schema.Primitive._

    {
      case PString     => "string"
      case PByte       => "byte"
      case PDouble     => "double"
      case PShort      => "short"
      case PUnit       => "unit"
      case PBigInt     => "bigInteger"
      case PInt        => "integer"
      case PUUID       => "uuid"
      case PLong       => "long"
      case PBoolean    => "boolean"
      case PFloat      => "float"
      case PBigDecimal => "bigDecimal"
      case PDocument   => "document"
      case PTimestamp  => "timestamp"
      case PBlob       => "blob"
    }
  }

  private def describeCollection[C[_]]: CollectionTag[C] => String = {
    import smithy4s.schema.CollectionTag._

    {
      case ListTag       => "list"
      case SetTag        => "set"
      case IndexedSeqTag => "@indexedSeq list"
      case VectorTag     => "@vector list"
    }
  }

  def describeSchema(schema: Schema[_]): () => String =
    schema match {
      case PrimitiveSchema(shapeId, _, tag) => now(s"${describePrimitive(tag)} ${shapeId.name}")

      case Schema.CollectionSchema(shapeId, _, tag, member) =>
        now(s"${describeCollection(tag)} ${shapeId.name} { member: ${describeSchema(member)()} }")

      case EnumerationSchema(shapeId, _, _, _) => now(s"enum ${shapeId.name}")

      case MapSchema(shapeId, _, key, value) =>
        now(s"map ${shapeId.name} { key: ${key.shapeId.name}, value: ${value.shapeId.name} }")

      case StructSchema(shapeId, _, _, _) => now(s"structure ${shapeId.name}")

      case UnionSchema(shapeId, _, _, _) => now(s"union ${shapeId.name}")

      case LazySchema(suspend) =>
        val desc = suspend.map(describeSchema)
        () => desc.value()

      case BijectionSchema(underlying, _, _) => describeSchema(underlying)

      case s => now(s.shapeId.name)
    }

  private def now(s: String): () => String = () => s

  sealed trait InsertUseClause extends Product with Serializable

  object InsertUseClause {
    case class Required(opsToServices: Map[OperationName, NonEmptyList[QualifiedIdentifier]])
      extends InsertUseClause
    case object NotRequired extends InsertUseClause
  }

  def forOperation[Op[_, _, _, _, _]](
    insertUseClause: InsertUseClause,
    endpoint: Endpoint[Op, _, _, _, _, _],
    serviceId: QualifiedIdentifier,
  ) = {
    val hints = endpoint.hints

    val useClauseOpt =
      insertUseClause match {
        case Required(_) =>
          TextEdit
            .Insert(
              (
                Formatter.renderUseClause(UseClause(serviceId)) + Doc.hardLine.repeat(2)
              ).render(Int.MaxValue),
              Position.origin,
            )
            .some
        case NotRequired => None
      }

    val fromServiceHint =
      insertUseClause match {
        case Required(opsToServices)
            // non-unique endpoint names need to be distinguished by service
            if opsToServices.get(OperationName(endpoint.name)).foldMap(_.toList).sizeIs > 1 =>
          s"(from ${Formatter.renderIdent(serviceId).render(Int.MaxValue)})"
        case _ => ""
      }

    CompletionItem(
      kind = CompletionItemKind.Function,
      label = endpoint.name,
      insertText = InsertText.JustString(endpoint.name),
      detail =
        s"$fromServiceHint: ${endpoint.input.shapeId.name} => ${endpoint.output.shapeId.name}",
      description = none,
      deprecated = hints.get(smithy.api.Deprecated).isDefined,
      docs = buildDocumentation(hints, isField = false),
      extraTextEdits = useClauseOpt.toList,
    )
  }

  def deprecationString(info: api.Deprecated): String = {
    val reasonString = info.message.foldMap(": " + _)
    val sinceString = info.since.foldMap(" (since " + _ + ")")

    sinceString ++ reasonString
  }

  def buildDocumentation(
    hints: Hints,
    isField: Boolean,
  ): Option[String] = {

    val deprecatedNote = hints.get(smithy.api.Deprecated).map { info =>
      s"**Deprecated**${deprecationString(info)}"
    }

    val optionalNote =
      hints.get(smithy.api.Required) match {
        case None if isField => "**Optional**".some
        case _               => none
      }

    List(
      deprecatedNote,
      optionalNote,
      hints.get(smithy.api.Http).map { http =>
        show"HTTP ${http.method.value} ${http.uri.value} "
      },
      hints.get(smithy.api.Documentation).map(_.value),
      hints.get(smithy.api.ExternalDocumentation).map(_.value).map {
        _.map { case (k, v) => show"""${k.value}: ${v.value}""" }.mkString("\n")
      },
    ).flatten.mkString("\n\n").some.filterNot(_.isEmpty)
  }

}

sealed trait CompletionItemKind extends Product with Serializable

object CompletionItemKind {
  case object EnumMember extends CompletionItemKind
  case object Field extends CompletionItemKind
  case object Constant extends CompletionItemKind
  case object UnionMember extends CompletionItemKind
  case object Function extends CompletionItemKind
}

object CompletionVisitor extends SchemaVisitor[CompletionResolver] {

  private def quoteAware[A](
    makeCompletion: (String => String) => List[CompletionItem]
  ): CompletionResolver[A] = {
    case PathEntry.Quotes :: Nil => makeCompletion(identity)
    case Nil                     => makeCompletion(TextUtils.quote)
    case _                       => Nil
  }

  override def primitive[P](
    shapeId: ShapeId,
    hints: Hints,
    tag: Primitive[P],
  ): CompletionResolver[P] =
    tag match {
      case Primitive.PTimestamp =>
        quoteAware { transformString =>
          val example = Timestamp.nowUTC().toString()

          CompletionItem.fromHints(
            CompletionItemKind.Constant,
            s"$example (now)",
            InsertText.JustString(transformString(example)),
            Schema.timestamp.addHints(hints).withId(shapeId),
          ) :: Nil
        }

      case Primitive.PUUID =>
        quoteAware { transformString =>
          val example = UUID.randomUUID().toString()

          CompletionItem.fromHints(
            CompletionItemKind.Constant,
            s"$example (random uuid)",
            InsertText.JustString(transformString(example)),
            Schema.timestamp.addHints(hints).withId(shapeId),
          ) :: Nil
        }

      case _ => _ => Nil
    }

  def collection[C[_], A](
    shapeId: ShapeId,
    hints: Hints,
    tag: CollectionTag[C],
    member: Schema[A],
  ): CompletionResolver[C[A]] =
    // todo in the future: for sets, exclude items already present (equal by AST)
    list(member).retag

  private def list[A](
    member: Schema[A]
  ): CompletionResolver[List[A]] = {
    val memberInstance = member.compile(this)

    {
      case PathEntry.CollectionEntry(_) :: rest => memberInstance.getCompletions(rest)
      case _                                    =>
        // other contexts are invalid
        Nil
    }
  }

  override def map[K, V](
    shapeId: ShapeId,
    hints: Hints,
    key: Schema[K],
    value: Schema[V],
  ): CompletionResolver[Map[K, V]] = {
    val fk = key.compile(this)
    val fv = value.compile(this)

    structLike(
      inBody = fk.getCompletions(Nil).map { item =>
        // adding " = " to underlying key's completion because it's not in the usual value position
        item.copy(
          insertText = InsertText.JustString(s"${item.label} = ")
        )

      },
      inValue = (_, t) => fv.getCompletions(t),
    )
  }

  override def enumeration[E](
    shapeId: ShapeId,
    hints: Hints,
    values: List[EnumValue[E]],
    total: E => EnumValue[E],
  ): CompletionResolver[E] = quoteAware { transformString =>
    values
      .map { enumValue =>
        CompletionItem.fromHints(
          CompletionItemKind.EnumMember,
          enumValue.name,
          InsertText.JustString(transformString(enumValue.name)),
          Schema.enumeration(total, values).addHints(hints).withId(shapeId),
        )
      }
  }

  private def structLike[S](
    inBody: List[CompletionItem],
    inValue: (String, List[PathEntry]) => List[CompletionItem],
  ): CompletionResolver[S] = {
    case PathEntry.StructBody :: Nil                              => inBody
    case PathEntry.StructBody :: PathEntry.StructValue(h) :: rest => inValue(h, rest)
    case _                                                        => Nil
  }

  override def struct[S](
    shapeId: ShapeId,
    hints: Hints,
    fields: Vector[SchemaField[S, _]],
    make: IndexedSeq[Any] => S,
  ): CompletionResolver[S] = {
    val compiledFields = fields.map(field => (field.mapK(this), field.instance))

    structLike(
      inBody =
        compiledFields
          // todo: filter out present fields
          .sortBy { case (field, _) => (field.isRequired, field.label) }
          .map(CompletionItem.fromField.tupled)
          .toList,
      inValue =
        (h, rest) =>
          compiledFields
            .collectFirst {
              case (field, _) if field.label === h => field
            }
            .foldMap(_.instance.getCompletions(rest)),
    )
  }

  override def union[U](
    shapeId: ShapeId,
    hints: Hints,
    alternatives: Vector[SchemaAlt[U, _]],
    dispatcher: Alt.Dispatcher[Schema, U],
  ): CompletionResolver[U] = {
    val allWithIds = alternatives.map { alt =>
      (alt.mapK(this), alt.instance)
    }

    structLike(
      inBody = allWithIds.map(CompletionItem.fromAlt.tupled).toList,
      inValue =
        (head, tail) =>
          allWithIds.find(_._1.label === head).toList.flatMap(_._1.instance.getCompletions(tail)),
    )
  }

  override def biject[A, B](schema: Schema[A], to: A => B, from: B => A): CompletionResolver[B] =
    schema.compile(this).retag

  override def surject[A, B](
    schema: Schema[A],
    to: Refinement[A, B],
    from: B => A,
  ): CompletionResolver[B] = schema.compile(this).retag

  // might need some testing
  override def lazily[A](suspend: Lazy[Schema[A]]): CompletionResolver[A] = {
    val underlying = suspend.map(_.compile(this))

    underlying.value.getCompletions(_)
  }

}
