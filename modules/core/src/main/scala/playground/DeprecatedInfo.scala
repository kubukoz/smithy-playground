package playground

import smithy.api

case class DeprecatedInfo(message: Option[String], since: Option[String])

object DeprecatedInfo {

  def fromHint(
    hint: api.Deprecated
  ): DeprecatedInfo = DeprecatedInfo(
    message = hint.message,
    since = hint.since,
  )

}
