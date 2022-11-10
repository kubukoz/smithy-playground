package playground

import aws.api
import playground.smithyql.QualifiedIdentifier
import smithy4s.Service
import smithyql.syntax._

object ServiceNameExtractor {

  def fromService[Alg[_[_, _, _, _, _]]](
    service: Service[Alg]
  ): QualifiedIdentifier = QualifiedIdentifier.fromShapeId(
    service.id.copy(name = service.hints.get(api.Service).map(_.sdkId).getOrElse(service.id.name))
  )

}
