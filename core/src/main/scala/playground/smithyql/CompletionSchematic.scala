package playground.smithyql

import smithy4s.schema.Alt
import smithy4s.schema.Field
import smithy4s.Hints
import smithy4s.schema.StubSchematic
import smithy4s.internals.Hinted
import smithy4s.ShapeId
import smithy4s.Lazy
import smithy4s.Endpoint
import cats.implicits._
import WithSource.NodeContext.PathEntry
import smithy4s.Timestamp
import smithy4s.Refinement
import org.typelevel.paiges.Doc
import cats.data.NonEmptyList
import playground.smithyql.CompletionItem.InsertUseClause.Required
import playground.smithyql.CompletionItem.InsertUseClause.NotRequired
import playground.TextUtils

object CompletionSchematic {
  type ResultR[+A] = List[PathEntry] => List[CompletionItem]
  type Result[A] = Hinted[ResultR, A]
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

  def fromHintedField[F[_]](field: Field[Hinted[F, *], _, _]): CompletionItem = fromHints(
    kind = CompletionItemKind.Field,
    label = field.label,
    insertText = InsertText.JustString(s"${field.label} = "),
    hints = field.instance.hints,
  )

  def fromHintedAlt[F[_]](alt: Alt[Hinted[F, *], _, _]): CompletionItem = fromHints(
    kind = CompletionItemKind.UnionMember,
    label = alt.label,
    insertText = InsertText.SnippetString(s"${alt.label} = {$$0},"),
    hints = alt.instance.hints,
  )

  def fromHints[F[_]](
    kind: CompletionItemKind,
    label: String,
    insertText: InsertText,
    hints: Hints,
  ): CompletionItem = CompletionItem(
    kind = kind,
    label = label,
    insertText = insertText,
    deprecated = hints.get(smithy.api.Deprecated).isDefined,
    detail = typeAnnotationShort(hints.get(ShapeId).get),
    description = hints.get(ShapeId).get.namespace.some,
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

final class CompletionSchematic extends StubSchematic[CompletionSchematic.Result] {
  import CompletionSchematic.Result
  import CompletionSchematic.ResultR

  def default[A]: Result[A] = Hinted.static[ResultR, A](_ => Nil)

  override val timestamp: Result[Timestamp] = Hinted[ResultR].from { hints =>
    {
      case Nil =>
        val example = Timestamp.nowUTC().toString()

        CompletionItem.fromHints(
          CompletionItemKind.Constant,
          s"$example (now)",
          InsertText.JustString(example),
          hints,
        ) :: Nil
      case _ => Nil
    }
  }

  private def retag[A, B](fs: Result[A]): Result[B] = fs.transform(identity(_): ResultR[B])

  override def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): Result[A] = Hinted[ResultR].from { hints =>
    def completeEnum(transformString: String => String) = fromOrdinal
      .toList
      .sortBy(_._1)
      .map(_._2)
      .map { enumValue =>
        val label = to(enumValue)._1

        CompletionItem.fromHints(
          CompletionItemKind.EnumMember,
          label,
          InsertText.JustString(transformString(label)),
          hints,
        )
      }

    {
      case PathEntry.Quotes :: Nil => completeEnum(identity)
      case Nil                     => completeEnum(TextUtils.quote)

      case _ =>
        // todo: this seems impossible tbh
        Nil
    }
  }

  override def map[K, V](
    fk: Result[K],
    fv: Result[V],
  ): Result[Map[K, V]] = Hinted.static[ResultR, Map[K, V]] {

    case PathEntry.StructBody :: Nil =>
      fk.get(Nil).map { item =>
        // adding " = " to underlying key's completion because it's not in the usual value position
        item.copy(
          insertText = InsertText.JustString(s"${item.label} = ")
        )

      }

    case _ =>
      // completions in map items not supported yet
      Nil

  }

  override def list[S](
    fs: Result[S]
  ): Result[List[S]] = Hinted.static[ResultR, List[S]] {
    case PathEntry.CollectionEntry :: rest => fs.get(rest)
    case _                                 =>
      // other contexts are invalid
      Nil
  }

  override def struct[S](
    fields: Vector[Field[Result, S, _]]
  )(
    const: Vector[Any] => S
  ): Result[S] = Hinted.static[ResultR, S] {
    case PathEntry.StructBody :: Nil =>
      fields
        // todo: filter out present fields
        .sortBy(field => (field.isRequired, field.label))
        .map(CompletionItem.fromHintedField(_))
        .toList

    case PathEntry.StructBody :: PathEntry.StructValue(h) :: rest =>
      fields.find(_.label === h).toList.flatMap(_.instance.get(rest))

    case _ =>
      // Non-structvalue context, we can neither complete nor descend
      Nil
  }

  override def union[S](
    first: Alt[Result, S, _],
    rest: Vector[Alt[Result, S, _]],
  )(
    total: S => Alt.WithValue[Result, S, _]
  ): Result[S] = Hinted.static[ResultR, S] {
    val all = rest.prepended(first)

    {
      case PathEntry.StructBody :: Nil => all.map(CompletionItem.fromHintedAlt(_)).toList

      case PathEntry.StructBody :: PathEntry.StructValue(head) :: tail =>
        all.find(_.label === head).toList.flatMap(_.instance.get(tail))

      case _ => Nil
    }

  }

  override def suspend[A](
    f: Lazy[Result[A]]
  ): Result[A] = Hinted.static[ResultR, A](in => f.value.get(in))

  override def bijection[A, B](f: Result[A], to: A => B, from: B => A): Result[B] = retag(f)

  override def surjection[A, B](
    f: Result[A],
    refinement: Refinement[A, B],
    from: B => A,
  ): Result[B] = retag(f)

  override def withHints[A](fa: Result[A], hints: Hints): Result[A] = fa.addHints(hints)
}
