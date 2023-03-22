package playground.smithyql

import cats.implicits._
import playground.ServiceNameExtractor
import smithy4s.Service
import smithy4s.ShapeId

object syntax {

  implicit final class QualifiedIdentifierOps(
    private val qi: QualifiedIdentifier
  ) extends AnyVal {
    def toShapeId: ShapeId = ShapeId(qi.renderNamespace, qi.selection)
  }

  implicit final class QualifiedIdentifierCompanionOps(
    private val ignored: QualifiedIdentifier.type
  ) extends AnyVal {

    def fromShapeId(
      shapeId: ShapeId
    ): QualifiedIdentifier = QualifiedIdentifier(
      shapeId.namespace.split("\\.").toList.toNel.getOrElse(sys.error("impossible! " + shapeId)),
      shapeId.name,
    )

    def forService[Alg[_[_, _, _, _, _]]](
      service: Service[Alg]
    ): QualifiedIdentifier = ServiceNameExtractor.fromService(service)

  }

}
