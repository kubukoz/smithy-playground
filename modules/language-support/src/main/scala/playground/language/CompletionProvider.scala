package playground.language

import cats.Id
import cats.implicits._
import playground.MultiServiceResolver
import playground.smithyql.NodeContext
import playground.smithyql.NodeContext.EmptyPath
import playground.smithyql.NodeContext.^^:
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.RangeIndex
import playground.smithyql.SourceFile
import playground.smithyql.WithSource
import playground.smithyql.parser.SourceParser
import playground.smithyql.syntax._
import smithy4s.dynamic.DynamicSchemaIndex

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

    // map of completions for each service.
    // the returned function takes a list of imported services (use clause)
    // and uses it to determine whether a new use clause is needed to use operations of this service.
    val completeOperationName
      : Map[QualifiedIdentifier, List[QualifiedIdentifier] => List[CompletionItem]] = servicesById
      .map { case (serviceId, service) =>
        serviceId -> { (presentServiceIdentifiers: List[QualifiedIdentifier]) =>
          val needsUseClause = !presentServiceIdentifiers.contains(serviceId)

          val insertUseClause =
            if (needsUseClause)
              CompletionItem.InsertUseClause.Required
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

    def completeRootOperationName(file: SourceFile[WithSource]) = {
      // todo: double-check test coverage.
      // there's definitely a test missing for N>1 clauses.
      val presentServiceIds
        : List[QualifiedIdentifier] = file.prelude.useClauses.map(_.value.identifier.value)

      // for operations on root level we show:
      // - completions for ops from the service being used, which don't insert a use clause and don't show the service ID
      // - completions for ops from other services, which insert a use clause and show the service IDs
      val notPresent = (servicesById.keySet -- presentServiceIds).toList

      (
        presentServiceIds ++
          notPresent
      ).flatMap(
        completeOperationName(_).apply(presentServiceIds)
      )
    }

    /* maps service ID to operation name to its input completions */
    val inputCompletions
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
      sf: SourceFile[WithSource],
      serviceId: Option[QualifiedIdentifier],
    ) =
      serviceId match {
        case Some(serviceId) =>
          completeOperationName(serviceId)(
            q.mapK(WithSource.unwrap).collectServiceIdentifiers
          )
        case None => completeRootOperationName(sf)
      }

    def completeInQuery(
      q: Query[WithSource],
      sf: SourceFile[WithSource],
      ctx: NodeContext,
    ): List[CompletionItem] = {
      // still wrong
      val serviceIdOpt =
        MultiServiceResolver
          .resolveService(
            q.mapK(WithSource.unwrap).collectServiceIdentifiers,
            serviceIdsById,
          )
          .toOption

      ctx match {
        case NodeContext.PathEntry.AtOperationName ^^: EmptyPath =>
          completeOperationNameFor(q, sf, serviceIdOpt)

        case NodeContext.PathEntry.AtOperationInput ^^: ctx =>
          serviceIdOpt match {
            case Some(serviceId) =>
              inputCompletions(serviceId)(
                q.operationName.value.operationName.value.mapK(WithSource.unwrap)
              )
                .getCompletions(ctx)

            case None => Nil
          }

        case _ => Nil
      }
    }

    (doc, pos) =>
      SourceParser[SourceFile].parse(doc) match {
        case Left(_) =>
          // we can try to deal with this later
          Nil

        case Right(sf) =>
          val matchingNode = RangeIndex
            .build(sf)
            .findAtPosition(pos)

          // System.err.println("matchingNode: " + matchingNode.render)

          matchingNode match {
            case NodeContext.PathEntry.InQuery(n) ^^: rest =>
              val q =
                sf
                  .queries(WithSource.unwrap)
                  .get(n.toLong)
                  .getOrElse(sys.error(s"Fatal error: no query at index $n"))
                  .query
                  .value

              completeInQuery(q, sf, rest)

            case NodeContext.PathEntry.AtPrelude ^^:
                NodeContext.PathEntry.AtUseClause(_) ^^:
                EmptyPath =>
              servicesById
                .toList
                .sortBy(_._1)
                .map(CompletionItem.useServiceClause.tupled)
                .toList

            case EmptyPath => completeRootOperationName(sf)
            case _         => Nil
          }

      }
  }

}
