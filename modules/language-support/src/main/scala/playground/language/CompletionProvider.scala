package playground.language

import cats.Id
import cats.implicits._
import playground.MultiServiceResolver
import playground.ServiceIndex
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

  def provide(
    documentText: String,
    pos: Position,
  ): List[CompletionItem]

}

object CompletionProvider {

  def forSchemaIndex(
    dsi: DynamicSchemaIndex
  ): CompletionProvider = forServices(dsi.allServices)

  def forServices(
    allServices: List[DynamicSchemaIndex.ServiceWrapper]
  ): CompletionProvider = {
    // long-term, it'd be nice to get rid of this (too low level)
    val servicesById =
      allServices.map { service =>
        QualifiedIdentifier.forService(service.service) -> service
      }.toMap

    val serviceIndex = ServiceIndex.fromServices(allServices)

    // Completions for the service's operations.
    // Uses a list of imported services (use clauses) to determine whether a new use clause is needed to use this service's operations.
    def completeOperationName(
      serviceId: QualifiedIdentifier,
      presentServiceIdentifiers: List[QualifiedIdentifier],
      insertBodyStruct: CompletionItem.InsertBodyStruct,
    ): List[CompletionItem] = {
      val needsUseClause = !presentServiceIdentifiers.contains_(serviceId)

      val insertUseClause =
        if (needsUseClause)
          CompletionItem.InsertUseClause.Required
        else
          CompletionItem.InsertUseClause.NotRequired

      servicesById(serviceId)
        .service
        .endpoints
        .map { e =>
          CompletionItem.forOperation(
            insertUseClause = insertUseClause,
            endpoint = e,
            serviceId = serviceId,
            insertBodyStruct = insertBodyStruct,
          )
        }
        .toList
    }

    def completeRootOperationName(
      file: SourceFile[WithSource],
      insertBodyStruct: CompletionItem.InsertBodyStruct,
    ) = {
      // double-check test coverage.
      // there's definitely a test missing for N>1 clauses.
      // https://github.com/kubukoz/smithy-playground/issues/161
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
        completeOperationName(_, presentServiceIds, insertBodyStruct)
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

    // we're definitely in an existing query, so we don't insert a brace in either case.
    def completeOperationNameFor(
      q: Query[WithSource],
      sf: SourceFile[WithSource],
      serviceId: Option[QualifiedIdentifier],
    ): List[CompletionItem] =
      serviceId match {
        case Some(serviceId) =>
          // includes the current query's service reference
          // as it wouldn't result in ading a use clause
          val presentServiceIdentifiers =
            q.operationName.value.mapK(WithSource.unwrap).identifier.toList ++
              sf.prelude.useClauses.map(_.value.identifier.value)

          completeOperationName(
            serviceId,
            presentServiceIdentifiers,
            CompletionItem.InsertBodyStruct.No,
          )

        case None => completeRootOperationName(sf, CompletionItem.InsertBodyStruct.No)
      }

    def completeInQuery(
      q: Query[WithSource],
      sf: SourceFile[WithSource],
      ctx: NodeContext,
    ): List[CompletionItem] = {
      val resolvedServiceId =
        MultiServiceResolver
          .resolveService(
            q.operationName.value,
            serviceIndex,
            sf.prelude.useClauses.map(_.value),
          )
          .toOption

      ctx match {
        case NodeContext.PathEntry.AtOperationName ^^: EmptyPath =>
          completeOperationNameFor(q, sf, resolvedServiceId)

        case NodeContext.PathEntry.AtOperationInput ^^: ctx =>
          resolvedServiceId match {
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

    (
      doc,
      pos,
    ) =>
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

            case EmptyPath => completeRootOperationName(sf, CompletionItem.InsertBodyStruct.Yes)

            case _ => Nil
          }

      }
  }

}
