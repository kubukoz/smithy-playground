package playground

import cats.Id
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.SourceRange
import smithy.api
import smithy4s.dynamic.DynamicSchemaIndex
import smithyql.syntax.*

// Abstraction for service metadata. Can be used by multi-service compilers/runners/completion providers etc.
trait ServiceIndex {

  def getService(
    id: QualifiedIdentifier
  ): Option[ServiceIndexEntry]

  // Note: this ignores missing services.
  def getServices(
    ids: Set[QualifiedIdentifier]
  ): List[ServiceIndexEntry]

  def serviceIds: Set[QualifiedIdentifier]
  def allServices: List[ServiceIndexEntry]
}

object ServiceIndex {

  val empty: ServiceIndex = fromServices(Nil)

  def fromServices(
    services: List[DynamicSchemaIndex.ServiceWrapper],
    getLocation: QualifiedIdentifier => Option[Location] = Function.const(None),
  ): ServiceIndex = {

    val serviceMeta: Map[QualifiedIdentifier, ServiceMetadata] =
      services.map { svc =>
        val ident = QualifiedIdentifier.forService(svc.service)

        ident ->
          ServiceMetadata(
            svc
              .service
              .endpoints
              .map { e =>
                OperationMetadata(
                  OperationName[Id](e.name),
                  location = getLocation(QualifiedIdentifier.fromShapeId(e.id)),
                )
              }
              .toSet,
            svc.service.hints.get[api.Deprecated].map(DeprecatedInfo.fromHint),
            location = getLocation(ident),
          )
      }.toMap

    fromMappings(serviceMeta)
  }

  private[playground] def fromMappings(
    mappings: Map[QualifiedIdentifier, ServiceMetadata]
  ): ServiceIndex =
    new ServiceIndex {

      private val entries = mappings.map { case (id, meta) =>
        id -> (ServiceIndexEntry.Entry(id, meta): ServiceIndexEntry)
      }

      val serviceIds: Set[QualifiedIdentifier] = mappings.keySet

      def getService(
        id: QualifiedIdentifier
      ): Option[ServiceIndexEntry] = entries.get(id)

      def getServices(
        ids: Set[QualifiedIdentifier]
      ): List[ServiceIndexEntry] = ids.toList.flatMap(entries.get)

      val allServices: List[ServiceIndexEntry] = entries.values.toList
    }

  private[playground] case class ServiceMetadata(
    operations: Set[OperationMetadata],
    deprecated: Option[DeprecatedInfo],
    location: Option[Location],
  )

  private[playground] case class OperationMetadata(
    name: OperationName[Id],
    location: Option[Location],
  ) extends ServiceIndexOperationEntry

}

trait ServiceIndexEntry {
  def id: QualifiedIdentifier
  def getOperation(name: OperationName[Id]): Option[ServiceIndexOperationEntry]
  def operationNames: Set[OperationName[Id]]

  def hasOperation(
    op: OperationName[Id]
  ): Boolean = getOperation(op).isDefined

  def deprecated: Option[DeprecatedInfo]

  def location: Option[Location]
}

object ServiceIndexEntry {

  private[playground] case class Entry(
    id: QualifiedIdentifier,
    metadata: ServiceIndex.ServiceMetadata,
  ) extends ServiceIndexEntry {
    def operationNames: Set[OperationName[Id]] = metadata.operations.map(_.name)

    def getOperation(name: OperationName[Id]): Option[ServiceIndexOperationEntry] = metadata
      .operations
      .find(_.name == name)

    export metadata.deprecated
    export metadata.location
  }

}

trait ServiceIndexOperationEntry {
  def name: OperationName[Id]
  def location: Option[Location]
}

case class Location(document: Uri, range: SourceRange.InFile)

object Location {

  val Empty: Location = Location(
    document = Uri.fromUriString("file://N/A"),
    range = SourceRange.InFile(
      start = Position.InFile(0, 0),
      end = Position.InFile(0, 0),
    ),
  )

}
