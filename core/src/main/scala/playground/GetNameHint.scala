package playground

import cats.Eval
import smithy4s.Hints
import smithy4s.ShapeId
import smithy4s.schema.StubSchematic
import smithy4s.internals.Hinted

object GetNameHint {
  type Result[A] = Hinted[Eval, String]

  val singleton = new GetNameHint
}

import GetNameHint._

class GetNameHint extends StubSchematic[Result] {

  def default[A]: Result[A] = Hinted[Eval].from { hints =>
    val hint = hints.get[ShapeId].get

    Eval.now(hint.name)
  }

  override def withHints[A](fa: Result[A], hints: Hints): Result[A] = fa.addHints(hints)

}
