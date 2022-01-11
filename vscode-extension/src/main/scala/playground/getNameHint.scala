package playground

import cats.Eval
import schematic.Field
import smithy4s.Hints
import smithy4s.ShapeId
import smithy4s.StubSchematic
import smithy4s.internals.Hinted

object getNameHint {
  type Result[A] = Hinted[Eval, String]
}

import getNameHint._

class getNameHint extends StubSchematic[Result] {
  def default[A]: Result[A] = Hinted(Hints(), _ => Eval.later(???))

  override def genericStruct[S](
    fields: Vector[Field[Result[*], S, _]]
  )(
    const: Vector[Any] => S
  ): Result[S] = Hinted[Eval].from { hints =>
    val hint = hints.get[ShapeId].get

    Eval.now(hint.name)
  }

  override def withHints[A](fa: Result[A], hints: Hints): Result[A] = fa.addHints(hints)

}
