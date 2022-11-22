package playground

import smithy4s.Service
import smithy4s.dynamic.DynamicSchemaIndex

object ServiceUtils {

  def wrapService[Algg[_[_, _, _, _, _]], Opp[_, _, _, _, _]](
    svc: Service[Algg, Opp]
  ): DynamicSchemaIndex.ServiceWrapper =
    new DynamicSchemaIndex.ServiceWrapper {
      type Alg[Oppp[_, _, _, _, _]] = Algg[Oppp]

      type Op[I, E, O, SE, SO] = Opp[I, E, O, SE, SO]
      val service: Service[Alg, Op] = svc
    }

}
