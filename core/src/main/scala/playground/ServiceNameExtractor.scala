package playground

import aws.api
import playground.smithyql.QualifiedIdentifier
import smithy4s.Service

object ServiceNameExtractor {

  def fromService[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): QualifiedIdentifier = QualifiedIdentifier.fromShapeId(
    service.id.copy(name = service.hints.get(api.Service).map(_.sdkId).getOrElse(service.id.name))
  )

}
