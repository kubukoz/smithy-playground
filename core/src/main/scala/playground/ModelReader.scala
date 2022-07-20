package playground

import smithy4s.dynamic.model.Model
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.http.PayloadError

object ModelReader {

  val modelParser: String => Either[PayloadError, Model] = {
    val capi = smithy4s.http.json.codecs()
    val codec = capi.compileCodec(Model.schema)

    text => capi.decodeFromByteArray(codec, text.getBytes())
  }

  def buildSchemaIndex(model: Model): DynamicSchemaIndex = DynamicSchemaIndex.load(model)
}
