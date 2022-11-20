package playground

import smithy.api.Default
import smithy4s.HintMask
import smithy4s.http.PayloadError

object BuildConfigDecoder {

  private val codec
    : (Array[Byte] => Either[PayloadError, BuildConfig], BuildConfig => Array[Byte]) = {
    // https://github.com/disneystreaming/smithy4s/issues/615
    val capi = smithy4s
      .http
      .json
      .codecs(smithy4s.http.json.codecs.defaultHintMask ++ HintMask(Default))
    val codec = capi.compileCodec(BuildConfig.schema)

    (capi.decodeFromByteArray(codec, _: Array[Byte]), capi.writeToArray(codec, _: BuildConfig))
  }

  val decode: Array[Byte] => Either[PayloadError, BuildConfig] = codec._1
  val encode: BuildConfig => Array[Byte] = codec._2

}
