package playground

import playground.smithyql.CompletionItem
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import smithy4s.Service

import smithyql.CompletionSchematic
import smithy4s.dynamic.DynamicSchemaIndex
import playground.smithyql.QualifiedIdentifier

trait CompletionProvider {
  def provide(documentText: String, pos: Position): List[CompletionItem]
}

object CompletionProvider {

  def forSchemaIndex[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    dsi: DynamicSchemaIndex
  ): CompletionProvider = {
    val completeOperationName =
      dsi
        .allServices
        .map { service =>
          QualifiedIdentifier.fromShapeId(service.service.id) ->
            service
              .service
              .endpoints
              .map(CompletionItem.forOperation)
        }
        .toMap

    val completionsByEndpoint
      : Map[QualifiedIdentifier, Map[OperationName, CompletionSchematic.ResultR[Any]]] =
      dsi
        .allServices
        .map { service =>
          QualifiedIdentifier.fromShapeId(service.service.id) ->
            service
              .service
              .endpoints
              .map { endpoint =>
                OperationName(endpoint.name) -> endpoint.input.compile(new CompletionSchematic).get
              }
              .toMap
        }
        .toMap

    (doc, pos) =>
      SmithyQLParser.parseFull(doc) match {
        // todo if one service, completeOperationName for it
        // if more services, completeOperationName which also inserts a "use"
        // if the operation name shows up in many services, show it twice?
        case Left(_) if doc.isBlank() => Nil /* completeOperationName */
        case Left(_)                  =>
          // we can try to deal with this later
          Nil

        case Right(q) =>
          val matchingNode = WithSource.atPosition(q)(pos)

          println("ctx at position: " + matchingNode)

          val serviceId =
            // todo: if many services and no clause, yield some failure
            q.useClause.fold(QualifiedIdentifier.fromShapeId(dsi.allServices.head.service.id)) {
              _.value.identifier
            }

          matchingNode
            .toList
            .flatMap {
              case WithSource.NodeContext.OperationContext(_) => completeOperationName(serviceId)

              case WithSource.NodeContext.InputContext(ctx) =>
                completionsByEndpoint(serviceId)(q.operationName.value)
                  .apply(ctx.toList)
            }
      }
  }

}
