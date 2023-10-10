package playground

import cats.data.Ior
import playground.smithyql.ContextRange
import playground.smithyql.NodeContext
import smithy4s.Blob

import scala.annotation.nowarn

object Diffs {
  import com.softwaremill.diffx._
  import com.softwaremill.diffx.cats._
  import com.softwaremill.diffx.generic.auto._

  implicit val diffNodeContext: Diff[NodeContext] = Diff.derivedDiff
  implicit val diffContextRange: Diff[ContextRange] = Diff.derivedDiff
  implicit val diffDiagnosticDetails: Diff[CompilationErrorDetails] = Diff.derivedDiff
  implicit val diffDiagnostic: Diff[CompilationError] = Diff.derivedDiff

  implicit val diffUnit: Diff[Unit] =
    (
      _,
      _,
      _,
    ) => IdenticalValue("unit")

  @nowarn("cat=unused")
  implicit def diffForIor[E: Diff, A: Diff]: Diff[Ior[E, A]] = Diff.derivedDiff

  implicit val diffByteArray: Diff[Blob] = Diff[String].contramap(_.toString())
  implicit val diffDocument: Diff[smithy4s.Document] = Diff.derivedDiff
  implicit val diffTimestamp: Diff[smithy4s.Timestamp] = Diff[String].contramap(_.toString())
}
