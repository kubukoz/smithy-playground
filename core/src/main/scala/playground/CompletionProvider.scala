package playground

import cats.Id
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
import playground.smithyql.WithSource
import smithy4s.dynamic.DynamicSchemaIndex

import smithyql.CompletionVisitor
import smithyql.CompletionResolver
import playground.smithyql.Query

trait CompletionProvider {
  def provide(documentText: String, pos: Position): List[CompletionItem]
}

object CompletionProvider {

  def forSchemaIndex(
    dsi: DynamicSchemaIndex
  ): CompletionProvider = forServices(dsi.allServices)

  def forServices(
    allServices: List[DynamicSchemaIndex.ServiceWrapper]
  ): CompletionProvider = {
    val servicesById =
      allServices.map { service =>
        QualifiedIdentifier.forService(service.service) -> service
      }.toMap

    val serviceIdsById = servicesById.map { case (k, _) => (k, k) }

    val opsToServices = servicesById.toList.foldMap { case (serviceId, service) =>
      service
        .service
        .endpoints
        .foldMap(e => Map(OperationName[Id](e.name) -> NonEmptyList.one(serviceId)))
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

    val completeAnyOperationName = completeOperationName.toList.map(_._2).flatSequence.apply(None)

    val completionsByEndpoint
      : Map[QualifiedIdentifier, Map[OperationName[Id], CompletionResolver[Any]]] = servicesById
      .fmap { service =>
        service
          .service
          .endpoints
          .map { endpoint =>
            OperationName[Id](endpoint.name) -> endpoint.input.compile(CompletionVisitor)
          }
          .toMap
      }

    def completeOperationNameFor(
      q: Query[WithSource],
      serviceId: Option[QualifiedIdentifier],
    ) =
      serviceId match {
        case Some(serviceId) =>
          completeOperationName(serviceId)(
            q.useClause.value.map(_.identifier.value)
          )
        case _ => completeAnyOperationName
      }

    (doc, pos) =>
      SmithyQLParser.parseFull(doc) match {
        case Left(_) if doc.isBlank() => completeAnyOperationName

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

          matchingNode
            .toList
            .flatMap {
              case NodeContext.PathEntry.AtUseClause ^^: Root =>
                servicesById
                  .toList
                  .sortBy(_._1)
                  .map(CompletionItem.useServiceClause.tupled)
                  .toList

              case NodeContext.PathEntry.AtOperationName ^^: Root =>
                completeOperationNameFor(q, serviceIdOpt)

              case NodeContext.PathEntry.AtOperationInput ^^: ctx =>
                serviceIdOpt match {
                  case Some(serviceId) =>
                    completionsByEndpoint(serviceId)(q.operationName.value.mapK(WithSource.unwrap))
                      .getCompletions(ctx)

                  case None => Nil
                }

              case _ => Nil
            }

      }
  }

}
