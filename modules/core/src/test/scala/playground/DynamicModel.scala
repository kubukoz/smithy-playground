package playground

import smithy4s.dynamic.DynamicSchemaIndex
import software.amazon.smithy.model.Model

object DynamicModel {

  def discover(
  ): DynamicSchemaIndex = {
    val model = Model
      .assembler()
      .discoverModels()
      .assemble()
      .unwrap()

    DynamicSchemaIndex.loadModel(model)
  }

}
