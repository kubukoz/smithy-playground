package playground.language

import cats.Monad
import cats.syntax.all.*
import playground.Location
import playground.MultiServiceResolver
import playground.ServiceIndex
import playground.Uri
import playground.smithyql.Position
import playground.smithyql.RunQuery
import playground.smithyql.SourceFile
import playground.smithyql.WithSource
import playground.smithyql.parser.SourceParser

trait DefinitionProvider[F[_]] {

  def definition(documentUri: Uri, position: Position): F[List[Location]]
}

object DefinitionProvider {

  def instance[F[_]: TextDocumentProvider: Monad](
    serviceIndex: ServiceIndex
  ): DefinitionProvider[F] = { (documentUri, position) =>
    TextDocumentProvider[F]
      .get(documentUri)
      .map { text =>
        SourceParser[SourceFile]
          .parse(text)
          .map { sf =>
            // Note: there's a bit of relatedness/duplication here and in the following:
            // - DocumentSymbolProvider
            // - RangeIndex
            // They all have different purposes in the end, but they pretty much navigate around the structure the same way.
            // Notably, DefinitionProvider is the only one that requires some semantic connection to the model, i.e. a ServiceIndex
            // and the other two are purely syntactic.
            // This one is also not indexed, i.e. we compute just the item we're interested in, and don't precompute all the symbols or ranges.
            // still, if Playground actually was a well written compiler, we could share a lot of logic here.

            val runQueries = sf
              .statements
              .value
              .collect { case RunQuery(query) => query.value.operationName }

            val forServiceName = List
              .concat(
                runQueries.flatMap(_.value.identifier),
                sf.prelude.useClauses.map(_.value.identifier),
              )
              .filter(_.range.contains(position))
              .map(_.value)
              .flatMap(serviceIndex.getService(_).flatMap(_.location))

            val forOperationName = runQueries
              .filter(_.value.operationName.range.contains(position))
              .flatMap { opName =>
                MultiServiceResolver
                  .resolveService(
                    queryOperationName = opName.value,
                    serviceIndex = serviceIndex,
                    useClauses = sf.prelude.useClauses.map(_.value),
                  )
                  .map { serviceId =>
                    List(
                      serviceId -> opName.value.operationName.value.mapK(WithSource.unwrap)
                    )
                  }
                  .getOrElse(Nil)
              }
              .flatMap { case (serviceId, opName) =>
                serviceIndex
                  .getService(serviceId)
                  .flatMap(_.getOperation(opName))
                  .flatMap(_.location)

              }

            forServiceName ++
              forOperationName
          }
      }
      .map(_.getOrElse(Nil))

  }

}
