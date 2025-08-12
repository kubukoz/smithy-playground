package playground.language

import cats.Monad
import cats.syntax.all.*
import playground.MultiServiceResolver
import playground.ServiceIndex
import playground.language.Uri
import playground.smithyql.Position
import playground.smithyql.RunQuery
import playground.smithyql.SourceFile
import playground.smithyql.SourceRange
import playground.smithyql.parser.SourceParser
import playground.std.PlaygroundSourceLocation
import smithy4s.Hints
import smithy4s.ShapeId
import smithy4s.dynamic.DynamicSchemaIndex

trait DefinitionProvider[F[_]] {

  def definition(documentUri: Uri, position: Position): F[List[Location]]
}

object DefinitionProvider {

  def instance[F[_]: TextDocumentProvider: Monad](
    dsi: DynamicSchemaIndex,
    serviceIndex: ServiceIndex,
  ): DefinitionProvider[F] = {
    def locationFromHints(hints: Hints): Option[Location] = hints
      .get[PlaygroundSourceLocation]
      .map { sourceLocation =>
        Location(
          document = Uri.fromUriString(sourceLocation.file match {
            // special-casing for the Smithy LSP
            case uri if uri.startsWith("jar:") => uri.replace("jar:", "smithyjar:")
            case uri                           => uri
          }),
          range = SourceRange.InFile(
            start = Position.InFile(
              sourceLocation.line - 1,
              sourceLocation.column - 1,
            ),
            end = Position.InFile(
              sourceLocation.line - 1,
              sourceLocation.column - 1,
            ),
          ),
        )
      }

    (documentUri, position) =>
      TextDocumentProvider[F]
        .get(documentUri)
        .map { text =>
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
                .filter(_.range.contains(position))
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

case class Location(document: Uri, range: SourceRange.InFile)
