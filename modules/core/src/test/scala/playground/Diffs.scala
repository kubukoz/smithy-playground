package playground

import playground.smithyql.ContextRange
import playground.smithyql.NodeContext
import cats.data.Ior
import scala.annotation.nowarn

object Diffs {
  import com.softwaremill.diffx._
  import com.softwaremill.diffx.generic.auto._
  import com.softwaremill.diffx.cats._

  implicit val diffNodeContext: Diff[NodeContext] = Diff.derivedDiff
  implicit val diffContextRange: Diff[ContextRange] = Diff.derivedDiff
  implicit val diffDiagnostic: Diff[CompilationError] = Diff.derivedDiff

  implicit val diffUnit: Diff[Unit] = (_, _, _) => IdenticalValue("unit")

  @nowarn("cat=unused")
  implicit def diffForIor[E: Diff, A: Diff]: Diff[Ior[E, A]] = Diff.derivedDiff
}
