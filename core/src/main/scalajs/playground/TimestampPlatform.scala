package playground

import smithy4s.Timestamp
import cats.implicits._

// js
object TimestampPlatform {

  // Workaround for smithy4s 0.13.x bug
  def fixupTimestamp(opt: Option[Timestamp]): Option[Timestamp] =
    opt match {
      case Some(s) if Either.catchNonFatal(s.toString()).isRight => Some(s)
      case _                                                     => None
    }

}
