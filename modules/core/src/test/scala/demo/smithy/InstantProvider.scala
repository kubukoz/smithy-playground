package demo.smithy

import smithy4s.Refinement
import smithy4s.RefinementProvider
import smithy4s.Surjection
import smithy4s.Timestamp

import java.time

object InstantProvider {

  implicit val provider: RefinementProvider[Instant, Timestamp, time.Instant] = Refinement
    .drivenBy[Instant](
      Surjection.catching((_: Timestamp).toInstant, Timestamp.fromInstant(_))
    )

}
