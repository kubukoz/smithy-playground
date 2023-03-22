package playground

import smithy4s.Service
import smithy4s.dynamic.DynamicSchemaIndex

object ServiceUtils {

  def wrapService[Algg[_[_, _, _, _, _]]](
    svc: Service[Algg]
  ): DynamicSchemaIndex.ServiceWrapper =
    new DynamicSchemaIndex.ServiceWrapper {
      type Alg[Oppp[_, _, _, _, _]] = Algg[Oppp]

      val service: Service[Alg] = svc
    }

}
