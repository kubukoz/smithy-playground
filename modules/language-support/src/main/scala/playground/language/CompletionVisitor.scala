package playground.language

import cats.Id
import cats.kernel.Eq
import cats.syntax.all.*
import playground.NodeEncoder
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
import smithy4s.Document
import smithy4s.Endpoint
import smithy4s.Hints
import smithy4s.Lazy
import smithy4s.Refinement
import smithy4s.ShapeId
import smithy4s.Timestamp
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.EnumTag
import smithy4s.schema.EnumTag.IntEnum
import smithy4s.schema.EnumValue
import smithy4s.schema.Field
import smithy4s.schema.Primitive
import smithy4s.schema.Schema
import smithy4s.schema.Schema.*
import smithy4s.schema.SchemaVisitor

import java.util.UUID

trait CompletionResolver[+A] {

  def getCompletions(
    ctx: NodeContext
  ): List[CompletionItem]

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

  def forNull: CompletionItem = CompletionItem(
    kind = CompletionItemKind.Constant,
    label = "null",
    insertText = InsertText.JustString("null"),
    detail = ": null",
    description = None,
    deprecated = false,
    docs = None,
    extraTextEdits = Nil,
    sortText = None,
  )

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
    field: Field[_, _]
  ): CompletionItem = fromHints(
    kind = CompletionItemKind.Field,
    label = field.label,
    insertText = InsertText.JustString(s"${field.label}: "),
    schema = field.schema,
  )

  def fromAlt(
    alt: Alt[_, _]
  ): CompletionItem = fromHints(
    kind = CompletionItemKind.UnionMember,
    label = alt.label,
    // needs proper completions for the inner schema
    // https://github.com/kubukoz/smithy-playground/pull/120
    insertText =
      if (describeSchema(alt.schema).apply().startsWith("structure "))
        InsertText.SnippetString(s"""${alt.label}: {
                                    |  $$0
                                    |},""".stripMargin)
      else
        InsertText.JustString(s"${alt.label}: "),
    alt.schema,
  )

  def fromHints(
    kind: CompletionItemKind,
    label: String,
    insertText: InsertText,
    schema: Schema[_],
    sortTextOverride: Option[String] = None,
  ): CompletionItem = {
    val isField = kind === CompletionItemKind.Field

    val sortText = sortTextOverride.orElse {
      isField match {
        case true if isRequiredField(schema) => Some(s"1_$label")
        case true                            => Some(s"2_$label")
        case false                           => None
      }
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
    import smithy4s.schema.Primitive.*

    {
      case PString     => "string"
      case PByte       => "byte"
      case PDouble     => "double"
      case PShort      => "short"
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

  private def describeCollection[C[_]](
    tag: CollectionTag[C],
    hints: Hints,
  ): String = {
    import smithy4s.schema.CollectionTag.*

    val base =
      tag match {
        case ListTag       => "list"
        case SetTag        => "set"
        case IndexedSeqTag => "@indexedSeq list"
        case VectorTag     => "@vector list"
      }

    sparseTraitDescription(hints).foldMap(_ + " ") + base
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

      case Schema.CollectionSchema(shapeId, hints, tag, member) =>
        now(
          s"${describeCollection(tag, hints)} ${shapeId.name} { member: ${describeSchema(member)()} }"
        )

      case e @ EnumerationSchema(_, _, _, _, _) =>
        e.tag match {
          case IntEnum() => now(s"intEnum ${e.shapeId.name}")
          case _         => now(s"enum ${e.shapeId.name}")
        }

      case MapSchema(shapeId, _, key, value) =>
        now(
          sparseTraitDescription(schema.hints).foldMap(_ + " ") +
            s"map ${shapeId.name} { key: ${describeSchema(key)()}, value: ${describeSchema(value)()} }"
        )

      case StructSchema(shapeId, _, _, _) => now(s"structure ${shapeId.name}")

      case UnionSchema(shapeId, _, _, _) => now(s"union ${shapeId.name}")

      case OptionSchema(underlying) =>
        // ignore the fact that it's nullable, just describe the underlying schema
        describeSchema(underlying)

      case LazySchema(suspend) =>
        // we don't look at fields or whatnot,
        // so we can immediately evaluate the schema and compile it as usual.
        describeSchema(suspend.value)

      case RefinementSchema(underlying, _) => describeSchema(underlying)

      case BijectionSchema(underlying, _) => describeSchema(underlying)
    }

  private def sparseTraitDescription(
    hints: Hints
  ): Option[String] = hints.get(api.Sparse).as("@sparse")

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

    {
      case PathEntry.CollectionEntry(_) ^^: rest => memberInstance.getCompletions(rest)
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

  override def option[A](
    schema: Schema[A]
  ): CompletionResolver[Option[A]] = {
    val underlying = schema.compile(this)

    {
      case p @ EmptyPath => underlying.getCompletions(p).appended(CompletionItem.forNull)
      case more          => underlying.getCompletions(more)
    }
  }

  override def enumeration[E](
    shapeId: ShapeId,
    hints: Hints,
    tag: EnumTag[E],
    values: List[EnumValue[E]],
    total: E => EnumValue[E],
  ): CompletionResolver[E] = quoteAware { transformString =>
    values
      .map { enumValue =>
        CompletionItem.fromHints(
          CompletionItemKind.EnumMember,
          enumValue.name,
          InsertText.JustString(transformString(enumValue.name)),
          Schema.enumeration(total, tag, values).addHints(hints).withId(shapeId),
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
    fields: Vector[Field[S, _]],
    make: IndexedSeq[Any] => S,
  ): CompletionResolver[S] = {
    // Artificial schema resembling this one. Should be pretty much equivalent.
    val schema = Schema.struct(fields)(make).addHints(hints).withId(shapeId)
    val documentDecoder = Document.Decoder.fromSchema(schema)

    val nodeEncoder = NodeEncoder.derive(schema)

    val compiledFields = fields.map(field => (field, field.schema.compile(this)))

    /* todo: pass this outside of Hints? (visitor context?) */
    val examples = hints
      .get(api.Examples)
      .foldMap(_.value)
      .zipWithIndex
      .flatMap { case (example, index) =>
        val name = example.title
        val doc = example.documentation

        for {
          input <- example.input
          decoded <- documentDecoder.decode(input).toOption
          // note: we could've transcoded from Document to Node directly, without the intermediate decoding
          // but the examples we suggest should be valid, and this is the only way to ensure that.
          encoded = nodeEncoder.toNode(decoded)

          // we're only covering inputs, and operation inputs must be structures.
          asObject <- encoded.asStruct
        } yield {
          val text = Formatter[Struct.Fields]
            .format(
              asObject
                .fields
                .mapK(WithSource.liftId),
              Int.MaxValue,
            )

          CompletionItem.fromHints(
            kind = CompletionItemKind.Constant /* todo */,
            label = s"Example: $name",
            insertText = InsertText.JustString(text),
            // issue: this doesn't work if the schema already has a Documentation hint. We should remove it first, or do something else.
            schema = schema.addHints(
              doc.map(api.Documentation(_)).map(Hints(_)).getOrElse(Hints.empty)
            ),
            sortTextOverride = Some(s"0_$index"),
          )

        }
      }

    structLike(
      inBody =
        examples ++ fields
          // todo: filter out present fields
          .sortBy(field => (field.isRequired && !field.hasDefaultValue, field.label))
          .map(CompletionItem.fromField)
          .toList,
      inValue =
        (
          h,
          rest,
        ) =>
          compiledFields
            .collectFirst {
              case (field, instance) if field.label === h => instance
            }
            .foldMap(_.getCompletions(rest)),
    )
  }

  override def union[U](
    shapeId: ShapeId,
    hints: Hints,
    alternatives: Vector[Alt[U, _]],
    dispatcher: Alt.Dispatcher[U],
  ): CompletionResolver[U] = {
    val compiledAlts = alternatives.map { alt =>
      (alt, alt.schema.compile(this))
    }

    structLike(
      inBody = alternatives.map(CompletionItem.fromAlt).toList,
      inValue =
        (
          head,
          tail,
        ) =>
          compiledAlts
            .collect {
              case (alt, instance) if alt.label === head => instance
            }
            .foldMap(_.getCompletions(tail)),
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
