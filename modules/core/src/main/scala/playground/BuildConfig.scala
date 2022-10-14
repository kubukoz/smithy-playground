package playground

import smithy4s.http.PayloadError

object BuildConfigDecoder {

  val decode: Array[Byte] => Either[PayloadError, BuildConfig] = {
    val capi = smithy4s.http.json.codecs()

    val codec = capi.compileCodec(BuildConfig.schema)

    capi.decodeFromByteArray(codec, _)
  }

}
