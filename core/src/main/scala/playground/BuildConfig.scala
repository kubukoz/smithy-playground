package playground

import smithy4s.schema.Schema
import smithy4s.http.PayloadError

case class BuildConfig(
  mavenDependencies: Option[List[String]],
  mavenRepositories: Option[List[String]],
  imports: Option[List[String]],
)

object BuildConfig {
  import Schema._

  val schema: Schema[BuildConfig] =
    struct(
      list(string).optional[BuildConfig]("mavenDependencies", _.mavenDependencies),
      list(string).optional[BuildConfig]("mavenRepositories", _.mavenRepositories),
      list(string).optional[BuildConfig]("imports", _.imports),
    )(BuildConfig.apply)

  val decode: Array[Byte] => Either[PayloadError, BuildConfig] = {
    val capi = smithy4s.http.json.codecs()

    val codec = capi.compileCodec(schema)

    capi.decodeFromByteArray(codec, _)
  }

}
