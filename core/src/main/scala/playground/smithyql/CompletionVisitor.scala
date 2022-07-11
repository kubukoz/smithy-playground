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
import smithy4s.schema.EnumValue
import smithy4s.schema.Field
import smithy4s.schema.Primitive
import smithy4s.schema.Primitive.PTimestamp
import smithy4s.schema.Schema
import smithy4s.schema.SchemaAlt
import smithy4s.schema.SchemaField
import smithy4s.schema.SchemaVisitor

import WithSource.NodeContext.PathEntry
import smithy4s.schema.CollectionTag
import smithy4s.schema.CollectionTag.IndexedSeqTag
import smithy4s.schema.CollectionTag.ListTag
import smithy4s.schema.CollectionTag.SetTag
import smithy4s.schema.CollectionTag.VectorTag

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

  def fromField(field: Field[CompletionResolver, _, _], schema: Schema[_]): CompletionItem =
    fromHints(
      kind = CompletionItemKind.Field,
      label = field.label,
      insertText = InsertText.JustString(s"${field.label} = "),
      hints = schema.hints,
      shapeId = schema.shapeId,
    )

  def fromAlt(alt: Alt[CompletionResolver, _, _], schema: Schema[_]): CompletionItem = fromHints(
    kind = CompletionItemKind.UnionMember,
    label = alt.label,
    // todo: unions aren't only for structs: this makes an invalid assumption
    // by inserting {} at all times
    insertText = InsertText.SnippetString(s"${alt.label} = {$$0},"),
    hints = schema.hints,
    shapeId = schema.shapeId,
  )

  def fromHints(
    kind: CompletionItemKind,
    label: String,
    insertText: InsertText,
    hints: Hints,
    shapeId: ShapeId,
  ): CompletionItem = CompletionItem(
    kind = kind,
    label = label,
    insertText = insertText,
    deprecated = hints.get(smithy.api.Deprecated).isDefined,
    detail = typeAnnotationShort(shapeId),
    description = shapeId.namespace.some,
    docs = buildDocumentation(hints),
    extraTextEdits = Nil,
  )

  def typeAnnotationShort(shapeId: ShapeId): String = s": ${shapeId.name}"

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
      docs = buildDocumentation(hints),
      extraTextEdits = useClauseOpt.toList,
    )
  }

  def buildDocumentation(hints: Hints): Option[String] = List(
    hints.get(smithy.api.Http).map { http =>
      show"HTTP ${http.method.value} ${http.uri.value} "
    },
    hints.get(smithy.api.Documentation).map(_.value),
    hints.get(smithy.api.ExternalDocumentation).map(_.value).map {
      _.map { case (k, v) => show"""${k.value}: ${v.value}""" }.mkString("\n")
    },
  ).flatten.mkString("\n\n").some.filterNot(_.isEmpty)

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

  override def primitive[P](
    shapeId: ShapeId,
    hints: Hints,
    tag: Primitive[P],
  ): CompletionResolver[P] =
    tag match {
      case PTimestamp =>
        def completeTimestamp(transformString: String => String) = {
          val example = Timestamp.nowUTC().toString()

          CompletionItem.fromHints(
            CompletionItemKind.Constant,
            s"$example (now)",
            InsertText.JustString(transformString(example)),
            hints,
            shapeId,
          ) :: Nil
        }

        {
          case PathEntry.Quotes :: Nil => completeTimestamp(identity)
          case Nil                     => completeTimestamp(TextUtils.quote)
          case _                       => Nil
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
  ): CompletionResolver[E] = {
    def completeEnum(transformString: String => String) = values
      .map { enumValue =>
        CompletionItem.fromHints(
          CompletionItemKind.EnumMember,
          enumValue.stringValue,
          InsertText.JustString(transformString(enumValue.stringValue)),
          hints,
          shapeId,
        )
      }

    val inQuotesCompletions = completeEnum(identity)
    val noQuotesCompletions = completeEnum(TextUtils.quote)

    {
      case PathEntry.Quotes :: Nil => inQuotesCompletions
      case Nil                     => noQuotesCompletions

      case _ =>
        // todo: this seems impossible tbh
        Nil
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
