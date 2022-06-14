package playground

import smithy4s.dynamic.model.Model
import smithy4s.dynamic.DynamicSchemaIndex

object ModelReader {

  val modelParser: String => Model = {
    val capi = smithy4s.http.json.codecs()
    val codec = capi.compileCodec(Model.schema)

    text => capi.decodeFromByteArray(codec, text.getBytes()).toTry.get
  }

  def buildSchemaIndex(model: Model): DynamicSchemaIndex = DynamicSchemaIndex.load(model)
}
