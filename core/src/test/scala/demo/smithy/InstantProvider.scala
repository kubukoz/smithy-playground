package demo.smithy

import smithy4s.Refinement
import smithy4s.Surjection
import smithy4s.Timestamp

object InstantProvider {

  val provider = Refinement.drivenBy[Instant](
    Surjection.catching((_: Timestamp).toInstant, Timestamp.fromInstant(_))
  )

}
