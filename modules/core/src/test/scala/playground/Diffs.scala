package playground

import cats.data.Chain
import com.softwaremill.diffx.instances.DiffForSeq
import playground.smithyql.ContextRange
import playground.smithyql.NodeContext

object Diffs {
  import com.softwaremill.diffx._
  import com.softwaremill.diffx.generic.auto._

  // TODO: wait for https://github.com/softwaremill/diffx/pull/413 to land
  // remove workaround below
  // import com.softwaremill.diffx.cats._

  implicit def diffChain[T: Diff](
    implicit
    seqMatcher: SeqMatcher[T]
  ): Diff[Chain[T]] =
    new DiffForSeq[Chain, T](
      Diff[T],
      seqMatcher,
      new SeqLike[Chain] {
        override def asSeq[A](c: Chain[A]): Seq[A] = c.toList
      },
      "Chain",
    )

  implicit val diffNodeContext: Diff[NodeContext] = Diff.derivedDiff
  implicit val diffContextRange: Diff[ContextRange] = Diff.derivedDiff
  implicit val diffDiagnostic: Diff[CompilationError] = Diff.derivedDiff
}
