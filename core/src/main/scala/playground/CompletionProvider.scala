package playground

import cats.data.NonEmptyList
import cats.implicits._
import playground.smithyql.CompletionItem
import playground.smithyql.NodeContext
import playground.smithyql.NodeContext.Root
import playground.smithyql.NodeContext.^^:
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.RangeIndex
import playground.smithyql.SmithyQLParser
import smithy4s.dynamic.DynamicSchemaIndex

import smithyql.CompletionVisitor
import smithyql.CompletionResolver

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

    val serviceIdsById = servicesById.map { case (k, _) => (k, k) }

    val opsToServices = servicesById.toList.foldMap { case (serviceId, service) =>
      service
        .service
        .endpoints
        .foldMap(e => Map(OperationName(e.name) -> NonEmptyList.one(serviceId)))
    }

    val completeOperationName = servicesById
      .map { case (serviceId, service) =>
        serviceId -> { (useClauseIdent: Option[QualifiedIdentifier]) =>
          val needsUseClause =
            MultiServiceResolver
              .resolveService(
                useClauseIdent,
                servicesById,
              )
              .isLeft

          val insertUseClause =
            if (needsUseClause)
              CompletionItem.InsertUseClause.Required(opsToServices)
            else
              CompletionItem.InsertUseClause.NotRequired

          service
            .service
            .endpoints
            .map { e =>
              CompletionItem.forOperation(
                insertUseClause = insertUseClause,
                endpoint = e,
                serviceId = serviceId,
              )
            }
        }
      }

    val completionsByEndpoint
      : Map[QualifiedIdentifier, Map[OperationName, CompletionResolver[Any]]] = servicesById
      .fmap { service =>
        service
          .service
          .endpoints
          .map { endpoint =>
            OperationName(endpoint.name) -> endpoint.input.compile(CompletionVisitor)
          }
          .toMap
      }

    (doc, pos) =>
      SmithyQLParser.parseFull(doc) match {
        case Left(_) if doc.isBlank() =>
          completeOperationName.toList.map(_._2).flatSequence.apply(None)

        case Left(_) =>
          // we can try to deal with this later
          Nil

        case Right(q) =>
          val matchingNode = RangeIndex
            .build(q)
            .findAtPosition(pos)
            .map(_.ctx)
          // println("matchingNode: " + matchingNode.map(_.render))

          val serviceIdOpt =
            MultiServiceResolver
              .resolveService(
                q.useClause.value.map(_.identifier.value),
                serviceIdsById,
              )
              .toOption

          serviceIdOpt match {
            case Some(serviceId) =>
              matchingNode
                .toList
                .flatMap {
                  case NodeContext.PathEntry.AtUseClause ^^: Root =>
                    servicesById.map(CompletionItem.useServiceClause.tupled).toList

                  case NodeContext.PathEntry.AtOperationName ^^: Root =>
                    completeOperationName(serviceId)(
                      q.useClause.value.map(_.identifier.value)
                    )

                  case NodeContext.PathEntry.AtOperationInput ^^: ctx =>
                    completionsByEndpoint(serviceId)(q.operationName.value)
                      .getCompletions(ctx)

                  case _ => Nil
                }

            case None =>
              // Compilation errors will be shown in the meantime
              Nil
          }

      }
  }

}
