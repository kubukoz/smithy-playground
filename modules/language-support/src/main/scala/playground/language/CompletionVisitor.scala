package playground.language

import cats.Id
import cats.implicits._
import cats.kernel.Eq
import playground.ServiceNameExtractor
import playground.TextUtils
import playground.language.CompletionItem.InsertUseClause.NotRequired
import playground.language.CompletionItem.InsertUseClause.Required
import playground.smithyql.NodeContext
import playground.smithyql.NodeContext.EmptyPath
import playground.smithyql.NodeContext.PathEntry
import playground.smithyql.NodeContext.^^:
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.QueryOperationName
import playground.smithyql.SourceRange
import playground.smithyql.Struct
import playground.smithyql.UseClause
import playground.smithyql.WithSource
import playground.smithyql.format.Formatter
import smithy.api
import smithy4s.Bijection
import smithy4s.Endpoint
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.EnumValue
import smithy4s.schema.Field
import smithy4s.schema.Primitive
import smithy4s.schema.Schema
import smithy4s.schema.Schema._
import smithy4s.schema.SchemaAlt
import smithy4s.schema.SchemaField
import smithy4s.schema.SchemaVisitor

import java.util.UUID

trait CompletionResolver[+A] {

  def getCompletions(
    ctx: NodeContext
  ): List[CompletionItem]

  def retag[B]: CompletionResolver[B] = getCompletions(_)
}

object CompletionResolver {

  def routed[A](
    f: NodeContext => CompletionResolver[A]
  ): CompletionResolver[A] = ctx => f(ctx).getCompletions(ctx)

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
  sortText: Option[String],
) {

  def asValueCompletion: CompletionItem = copy(
    insertText = InsertText.JustString(s"$label = ")
  )

}

sealed trait TextEdit extends Product with Serializable

object TextEdit {

  final case class Insert(
    text: String,
    pos: Position,
  ) extends TextEdit

  final case class Overwrite(
    text: String,
    range: SourceRange,
  ) extends TextEdit

}

sealed trait InsertText extends Product with Serializable

object InsertText {

  final case class JustString(
    value: String
  ) extends InsertText

  final case class SnippetString(
    value: String
  ) extends InsertText

}

object CompletionItem {

  def useServiceClause(
    ident: QualifiedIdentifier,
    service: DynamicSchemaIndex.ServiceWrapper,
  ): CompletionItem = fromHints(
    kind = CompletionItemKind.Module,
    label = ident.selection,
    insertText = InsertText.JustString(
      Formatter.writeIdentifier(ident, Int.MaxValue)
    ),
    schema = Schema.unit.addHints(service.service.hints),
  ).copy(detail = describeService(service))

  def fromField(
    field: Field[Schema, _, _]
  ): CompletionItem = fromHints(
    kind = CompletionItemKind.Field,
    label = field.label,
    insertText = InsertText.JustString(s"${field.label}: "),
    schema = field.instance,
  )

  def fromAlt(
    alt: Alt[Schema, _, _]
  ): CompletionItem = fromHints(
    kind = CompletionItemKind.UnionMember,
    label = alt.label,
    // needs proper completions for the inner schema
    // https://github.com/kubukoz/smithy-playground/pull/120
    insertText =
      if (describeSchema(alt.instance).apply().startsWith("structure "))
        InsertText.SnippetString(s"""${alt.label}: {
                                    |  $$0
                                    |},""".stripMargin)
      else
        InsertText.JustString(s"${alt.label}: "),
    alt.instance,
  )

  def fromHints(
    kind: CompletionItemKind,
    label: String,
    insertText: InsertText,
    schema: Schema[_],
  ): CompletionItem = {
    val isField = kind === CompletionItemKind.Field

    val sortText =
      isField match {
        case true if isRequiredField(schema) => Some(s"1_$label")
        case true                            => Some(s"2_$label")
        case false                           => None
      }

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
      sortText = sortText,
    )
  }

  def describeType(
    isField: Boolean,
    schema: Schema[_],
  ): String = {
    val isOptional = isField && !isRequiredField(schema)

    val optionalPrefix =
      if (isOptional)
        "?"
      else
        ""
    show"$optionalPrefix: ${describeSchema(schema)()}"
  }

  private def isRequiredField(
    schema: Schema[_]
  ): Boolean = schema.hints.has(smithy.api.Required)

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

  def describeService(
    service: DynamicSchemaIndex.ServiceWrapper
  ): String = s": service ${ServiceNameExtractor.fromService(service.service).selection}"

  // nice to have: precompile this? caching?
  def describeSchema(
    schema: Schema[_]
  ): (
  ) => String =
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
        // we don't look at fields or whatnot,
        // so we can immediately evaluate the schema and compile it as usual.
        describeSchema(suspend.value)

      case RefinementSchema(underlying, _) => describeSchema(underlying)

      case BijectionSchema(underlying, _) => describeSchema(underlying)
    }

  private def now(
    s: String
  ): (
  ) => String =
    (
    ) => s

  sealed trait InsertUseClause extends Product with Serializable

  object InsertUseClause {
    case object Required extends InsertUseClause
    case object NotRequired extends InsertUseClause
  }

  sealed trait InsertBodyStruct extends Product with Serializable

  object InsertBodyStruct {
    case object Yes extends InsertBodyStruct
    case object No extends InsertBodyStruct
  }

  def forOperation[Op[_, _, _, _, _]](
    insertUseClause: InsertUseClause,
    endpoint: Endpoint[Op, _, _, _, _, _],
    serviceId: QualifiedIdentifier,
    insertBodyStruct: InsertBodyStruct,
  ): CompletionItem = {
    val hints = endpoint.hints

    val useClauseOpt =
      insertUseClause match {
        case Required =>
          TextEdit
            .Insert(
              Formatter
                .useClauseFormatter
                .format(UseClause[Id](serviceId).mapK(WithSource.liftId), Int.MaxValue) +
                "\n\n",
              Position.origin,
            )
            .some
        case NotRequired => None
      }

    val fromServiceHint =
      insertUseClause match {
        case Required => s"(from ${Formatter.writeIdentifier(serviceId, Int.MaxValue)})"
        case _        => ""
      }

    val label = endpoint.name

    val renderedBody = Formatter[Query]
      .format(
        Query[Id](
          operationName = QueryOperationName[Id](
            identifier = None,
            operationName = OperationName[Id](endpoint.name),
          ),
          input = Struct[Id](Struct.Fields.empty[Id]),
        ).mapK(WithSource.liftId),
        Int.MaxValue,
      )
    val insertText =
      insertBodyStruct match {
        case InsertBodyStruct.Yes =>
          InsertText.SnippetString(
            renderedBody
              // Kinda hacky - a way to place the cursor at the right position.
              // Not sure how to do this best while using the formatter... (maybe some sort of mechanism inside the formatter to inject things).
              // This assumes operation completions are always on the top level (no indent expected).
              .replace("\n}", "  $0\n}")
          )
        case InsertBodyStruct.No => InsertText.JustString(endpoint.name)
      }

    CompletionItem(
      kind = CompletionItemKind.Function,
      label = label,
      insertText = insertText,
      detail =
        s"$fromServiceHint: ${endpoint.input.shapeId.name} => ${endpoint.output.shapeId.name}",
      description = none,
      deprecated = hints.get(smithy.api.Deprecated).isDefined,
      docs = buildDocumentation(hints, isField = false),
      extraTextEdits = useClauseOpt.toList,

      // giving priority to the entries that don't require a use clause
      // (they're considered to be in scope)
      sortText = Some(insertUseClause match {
        case NotRequired => "1_"
        case Required    => "2_"
      }).map(_ + label),
    )
  }

  def deprecationString(
    info: api.Deprecated
  ): String = {
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
  case object Module extends CompletionItemKind
  case object Constant extends CompletionItemKind
  case object UnionMember extends CompletionItemKind
  case object Function extends CompletionItemKind

  implicit val eq: Eq[CompletionItemKind] = Eq.fromUniversalEquals
}

object CompletionVisitor extends SchemaVisitor[CompletionResolver] {

  private def quoteAware[A](
    makeCompletion: (
      String => String
    ) => List[CompletionItem]
  ): CompletionResolver[A] = {
    case PathEntry.Quotes ^^: EmptyPath => makeCompletion(identity)
    case EmptyPath                      => makeCompletion(TextUtils.quote)
    case _                              => Nil
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

    val emptyList = CompletionItem(
      kind = CompletionItemKind.Constant /* todo */,
      label = "[]",
      insertText = InsertText.SnippetString("[$0]"),
      detail = "todo",
      description = Some("todo"),
      deprecated = false,
      sortText = None,
      // todo
      docs = None,
      extraTextEdits = Nil,
    )

    {
      case PathEntry.CollectionEntry(_) ^^: rest => memberInstance.getCompletions(rest)
      case EmptyPath                             => List(emptyList)
      case _                                     =>
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
      inBody = fk.getCompletions(NodeContext.EmptyPath).map { item =>
        item.asValueCompletion
      },
      inValue =
        (
          _,
          t,
        ) => fv.getCompletions(t),
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
    inValue: (
      String,
      NodeContext,
    ) => List[CompletionItem],
  ): CompletionResolver[S] = {
    case PathEntry.StructBody ^^: EmptyPath                         => inBody
    case PathEntry.StructBody ^^: PathEntry.StructValue(h) ^^: rest => inValue(h, rest)
    case _                                                          => Nil
  }

  override def struct[S](
    shapeId: ShapeId,
    hints: Hints,
    fields: Vector[SchemaField[S, _]],
    make: IndexedSeq[Any] => S,
  ): CompletionResolver[S] = {
    val emptyStruct = CompletionItem(
      kind = CompletionItemKind.Constant /* todo */,
      label = "{}",
      insertText = InsertText.SnippetString("{$0}"),
      detail = "todo",
      description = Some("todo"),
      deprecated = false,
      sortText = None,
      // todo
      docs = None,
      extraTextEdits = Nil,
    )

    val compiledFields = fields.map(field => (field.mapK(this), field.instance))

    val byStructShape = structLike(
      inBody =
        fields
          // todo: filter out present fields
          .sortBy(field => (field.isRequired, field.label))
          .map(CompletionItem.fromField)
          .toList,
      inValue =
        (
          h,
          rest,
        ) =>
          compiledFields
            .collectFirst {
              case (field, _) if field.label === h => field
            }
            .foldMap(_.instance.getCompletions(rest)),
    )

    // todo this doesn't work
    CompletionResolver.routed {
      case EmptyPath => _ => List(emptyStruct)
      case _         => byStructShape
    }
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
      inBody = alternatives.map(CompletionItem.fromAlt).toList,
      inValue =
        (
          head,
          tail,
        ) => allWithIds.find(_._1.label === head).toList.flatMap(_._1.instance.getCompletions(tail)),
    )
  }

  override def biject[A, B](
    schema: Schema[A],
    bijection: Bijection[A, B],
  ): CompletionResolver[B] = schema.compile(this).retag

  override def refine[A, B](
    schema: Schema[A],
    refinement: Refinement[A, B],
  ): CompletionResolver[B] = schema.compile(this).retag

  // might need some testing
  override def lazily[A](
    suspend: Lazy[Schema[A]]
  ): CompletionResolver[A] = {
    val underlying = suspend.map(_.compile(this))

    underlying.value.getCompletions(_)
  }

}
