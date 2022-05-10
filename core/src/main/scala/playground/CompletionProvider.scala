package playground

import playground.smithyql.CompletionItem
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import cats.implicits._
import smithyql.CompletionSchematic
import smithy4s.dynamic.DynamicSchemaIndex
import playground.smithyql.QualifiedIdentifier
import cats.data.NonEmptyList

trait CompletionProvider {
  def provide(documentText: String, pos: Position): List[CompletionItem]
}

object CompletionProvider {

  def forSchemaIndex[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    dsi: DynamicSchemaIndex
  ): CompletionProvider = {
    val servicesById =
      dsi
        .allServices
        .map { service =>
          QualifiedIdentifier.fromShapeId(service.service.id) -> service
        }
        .toMap

    val opsToServices = servicesById.toList.foldMap { case (serviceId, service) =>
      service
        .service
        .endpoints
        .foldMap(e => Map(OperationName(e.name) -> NonEmptyList.one(serviceId)))
    }

    val completeOperationName = servicesById
      .map { case (serviceId, service) =>
        serviceId -> { (needsUseClause: Boolean) =>
          service
            .service
            .endpoints
            .map { e =>
              CompletionItem.forOperation(
                needsUseClause = needsUseClause,
                endpoint = e,
                serviceId = serviceId,
                opsToServices = opsToServices,
              )
            }
        }
      }

    val completionsByEndpoint
      : Map[QualifiedIdentifier, Map[OperationName, CompletionSchematic.ResultR[Any]]] =
      servicesById
        .fmap { service =>
          service
            .service
            .endpoints
            .map { endpoint =>
              OperationName(endpoint.name) -> endpoint.input.compile(new CompletionSchematic).get
            }
            .toMap
        }

    (doc, pos) =>
      SmithyQLParser.parseFull(doc) match {
        case Left(_) if doc.isBlank() =>
          if (completeOperationName.sizeIs == 1)
            // one service only - use clause not necessary
            completeOperationName.head._2(false)
          else {
            // todo: if the operation name shows up in many services, show it twice with explicit service name?
            completeOperationName.toList.map(_._2).flatSequence.apply(true)
          }
        case Left(_) =>
          // we can try to deal with this later
          Nil

        case Right(q) =>
          val matchingNode = WithSource.atPosition(q)(pos)

          println("ctx at position: " + matchingNode)

          val serviceId =
            // todo: if many services and no clause, yield some failure
            q.useClause.fold(servicesById.head._1) {
              _.value.identifier
            }

          matchingNode
            .toList
            .flatMap {
              case WithSource.NodeContext.OperationContext(_) =>
                completeOperationName(serviceId)(
                  // when the clause is missing and necessary
                  q.useClause.isEmpty && dsi.allServices.sizeIs > 1
                )

              case WithSource.NodeContext.InputContext(ctx) =>
                completionsByEndpoint(serviceId)(q.operationName.value)
                  .apply(ctx.toList)
            }
      }
  }

}
