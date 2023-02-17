package playground

import cats.Id
import cats.implicits._
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import smithy.api
import smithy4s.dynamic.DynamicSchemaIndex
import smithyql.syntax._

// Abstraction for service metadata. Can be used by multi-service compilers/runners/completion providers etc.
trait ServiceIndex {
  def getService(id: QualifiedIdentifier): Option[ServiceIndexEntry]
  // Note: this ignores missing services.
  def getServices(ids: Set[QualifiedIdentifier]): List[ServiceIndexEntry]
  def serviceIds: Set[QualifiedIdentifier]
  def allServices: List[ServiceIndexEntry]
}

object ServiceIndex {

  val empty: ServiceIndex = fromServices(Nil)

  def fromServices(
    services: List[DynamicSchemaIndex.ServiceWrapper]
  ): ServiceIndex = {

    val serviceMeta: Map[QualifiedIdentifier, ServiceMetadata] =
      services.map { svc =>
        QualifiedIdentifier.forService(svc.service) ->
          ServiceMetadata(
            svc
              .service
              .endpoints
              .map(_.name)
              .map(OperationName[Id](_))
              .toSet,
            svc.service.hints.get(api.Deprecated).map(DeprecatedInfo.fromHint),
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
    operationNames: Set[OperationName[Id]],
    deprecated: Option[DeprecatedInfo],
  )

}

trait ServiceIndexEntry {
  def id: QualifiedIdentifier
  def operationNames: Set[OperationName[Id]]
  def hasOperation(op: OperationName[Id]): Boolean
  def deprecated: Option[DeprecatedInfo]
}

object ServiceIndexEntry {

  private[playground] case class Entry(
    id: QualifiedIdentifier,
    metadata: ServiceIndex.ServiceMetadata,
  ) extends ServiceIndexEntry {
    def operationNames: Set[OperationName[Id]] = metadata.operationNames

    def hasOperation(op: OperationName[Id]): Boolean = operationNames.contains_(op)

    def deprecated: Option[DeprecatedInfo] = metadata.deprecated
  }

}
