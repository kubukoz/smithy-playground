package playground.lsp

import cats.Monad
import cats.parse.LocationMap
import cats.syntax.all.*
import playground.MultiServiceResolver
import playground.ServiceIndex
import playground.language.Uri
import playground.smithyql.RunQuery
import playground.smithyql.SourceFile
import playground.smithyql.parser.SourceParser
import playground.std.PlaygroundSourceLocation
import smithy4s.Hints
import smithy4s.ShapeId
import smithy4s.dynamic.DynamicSchemaIndex

// todo move to language-support
trait DefinitionProvider[F[_]] {

  def definition(documentUri: Uri, position: LSPPosition): F[List[LSPLocation]]
}

object DefinitionProvider {

  def instance[F[_]: TextDocumentManager: Monad](
    dsi: DynamicSchemaIndex,
    serviceIndex: ServiceIndex,
  ): DefinitionProvider[F] = {
    def locationFromHints(hints: Hints): Option[LSPLocation] = hints
      .get[PlaygroundSourceLocation]
      .map { sourceLocation =>
        LSPLocation(
          document = Uri.fromUriString(sourceLocation.file match {
            // special-casing for the Smithy LSP
            case uri if uri.startsWith("jar:") => uri.replace("jar:", "smithyjar:")
            case uri                           => uri
          }),
          range = LSPRange(
            from = LSPPosition(
              sourceLocation.line - 1,
              sourceLocation.column - 1,
            ),
            to = LSPPosition(
              sourceLocation.line - 1,
              sourceLocation.column - 1,
            ),
          ),
        )
      }

    (documentUri, position) =>
      TextDocumentManager[F]
        .get(documentUri)
        .map { text =>
          val map = LocationMap(text)

          val pos = position.unwrap(map)

          SourceParser[SourceFile]
            .parse(text)
            .map { sf =>
              val runQueries = sf
                .statements
                .value
                .collect { case RunQuery(query) => query.value.operationName }

              val forServiceName = List
                .concat(
                  runQueries.flatMap(_.value.identifier),
                  sf.prelude.useClauses.map(_.value.identifier),
                )
                .filter(_.range.contains(pos))
                .map(_.value)
                .flatMap { serviceId =>
                  val srv = ShapeId(serviceId.renderNamespace, serviceId.selection)

                  dsi
                    .getService(srv)
                    .toList
                    .map(_.service)
                    .flatMap { service =>
                      locationFromHints(service.hints)
                    }
                }

              val forOperationName = runQueries
                .filter(_.value.operationName.range.contains(pos))
                .flatMap { opName =>
                  MultiServiceResolver
                    .resolveService(
                      queryOperationName = opName.value,
                      serviceIndex = serviceIndex,
                      useClauses = sf.prelude.useClauses.map(_.value),
                    )
                    .map { serviceId =>
                      List(
                        serviceId -> opName.value.operationName.value.text
                      )
                    }
                    .getOrElse(Nil)
                }
                .flatMap { case (serviceId, opName) =>
                  dsi
                    .getService(
                      ShapeId(serviceId.renderNamespace, serviceId.selection)
                    )
                    .toList
                    .map(_.service)
                    .flatMap { srv =>
                      srv.endpoints.find(_.name == opName)
                    }
                    .flatMap { endpoint =>
                      locationFromHints(endpoint.hints)
                    }

                }

              forServiceName ++
                forOperationName
            }
        }
        .map(_.getOrElse(Nil))

  }

}
