package playground.smithyql

import schematic.Alt
import schematic.Field
import smithy4s.Hints
import smithy4s.StubSchematic
import smithy4s.internals.Hinted
import smithy.api
import smithy4s.ShapeId

object CompletionSchematic {
  // from context
  type ResultR[+A] = List[String] => List[CompletionItem]
  type Result[A] = Hinted[ResultR, A]
}

sealed trait CompletionItem extends Product with Serializable

object CompletionItem {
  final case class Field(label: String, tpe: String) extends CompletionItem
  final case class UnionMember(label: String, deprecated: Boolean, tpe: String)
    extends CompletionItem
}

final class CompletionSchematic extends StubSchematic[CompletionSchematic.Result] {
  import CompletionSchematic.Result
  import CompletionSchematic.ResultR

  def default[A]: Result[A] = Hinted.static[ResultR, A](_ => Nil)

  override def struct[S](
    fields: Vector[Field[Result, S, _]]
  )(
    const: Vector[Any] => S
  ): Result[S] = Hinted.static[ResultR, S] {
    case Nil =>
      fields
        .sortBy(field => (field.isRequired, field.label))
        .map { field =>
          CompletionItem.Field(field.label, tpe = field.instance.hints.get(ShapeId).get.show)
        }
        .toList

    case h :: rest => fields.find(_.label == h).toList.flatMap(_.instance.get(rest))
  }

  override def union[S](
    first: Alt[Result, S, _],
    rest: Vector[Alt[Result, S, _]],
  )(
    total: S => Alt.WithValue[Result, S, _]
  ): Result[S] = Hinted.static[ResultR, S] {
    val all = rest.prepended(first)

    {
      case head :: tail => all.find(_.label == head).toList.flatMap(_.instance.get(tail))

      case Nil =>
        all.map { field =>
          // todo: add type
          CompletionItem.UnionMember(
            field.label,
            deprecated = field.instance.hints.get(api.Deprecated).isDefined,
            tpe = field.instance.hints.get(ShapeId).get.show,
          )
        }.toList
    }

  }

  override def bijection[A, B](f: Result[A], to: A => B, from: B => A): Result[B] = f.transform(
    identity(_): ResultR[B]
  )

  override def withHints[A](fa: Result[A], hints: Hints): Result[A] = fa.addHints(hints)

}
