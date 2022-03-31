package playground.smithyql

import schematic.Alt
import schematic.Field
import smithy4s.Hints
import smithy4s.StubSchematic

object CompletionSchematic {
  // from context
  type Result[+A] = List[String] => List[String]
}

final class CompletionSchematic extends StubSchematic[CompletionSchematic.Result] {
  import CompletionSchematic.Result

  def default[A]: Result[A] = _ => Nil

  override def struct[S](
    fields: Vector[Field[Result, S, _]]
  )(
    const: Vector[Any] => S
  ): Result[S] = {
    case Nil       => fields.sortBy(field => (field.isRequired, field.label)).map(_.label).toList
    case h :: rest => fields.find(_.label == h).toList.flatMap(_.instance(rest))
  }

  override def union[S](
    first: Alt[Result, S, _],
    rest: Vector[Alt[Result, S, _]],
  )(
    total: S => Alt.WithValue[Result, S, _]
  ): Result[S] = {
    val all = rest.prepended(first)

    {
      case head :: tail => all.find(_.label == head).toList.flatMap(_.instance(tail))

      case Nil => all.map(_.label).toList
    }

  }

  override def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): Result[A] = {
    // todo: the input should probably be something like List(stringLiteral) for this to happen
    case Nil => fromName.keySet.toList
    case _   => Nil
  }

  override def bijection[A, B](f: Result[A], to: A => B, from: B => A): Result[B] = f

  override def withHints[A](fa: Result[A], hints: Hints): Result[A] = fa

}