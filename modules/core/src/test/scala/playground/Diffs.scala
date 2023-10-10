package playground

import cats.data.Ior
import playground.smithyql.ContextRange
import playground.smithyql.Diffs.given
import playground.smithyql.NodeContext
import smithy.api.TimestampFormat
import smithy4s.Blob
import smithy4s.ShapeId

import scala.annotation.nowarn

object Diffs {
  import com.softwaremill.diffx._
  import com.softwaremill.diffx.cats._

  given Diff[ShapeId] = Diff.derived

  given Diff[NodeContext.PathEntry] = Diff.derived
  given Diff[NodeContext] = Diff[List[NodeContext.PathEntry]].contramap(_.toList)
  given Diff[ContextRange] = Diff.derived
  given Diff[TimestampFormat] = Diff.derived
  given Diff[DiagnosticTag] = Diff.derived
  given Diff[DiagnosticSeverity] = Diff.derived
  given Diff[DeprecatedInfo] = Diff.derived
  given Diff[CompilationErrorDetails] = Diff.derived
  given Diff[CompilationError] = Diff.derived

  given Diff[Unit] =
    (
      _,
      _,
      _,
    ) => IdenticalValue("unit")

  @nowarn("cat=unused")
  given [E: Diff, A: Diff]: Diff[Ior[E, A]] = Diff.derived

  given Diff[Blob] = Diff[String].contramap(_.toUTF8String)
  given Diff[smithy4s.Document] = Diff.derived
  given Diff[smithy4s.Timestamp] = Diff[String].contramap(_.toString())
}
