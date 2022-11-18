package playground

import cats.Id
import cats.implicits._
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import smithy4s.dynamic.DynamicSchemaIndex

import smithyql.syntax._

// Abstraction for service metadata. Can be used by multi-service compilers/runners/completion providers etc.
trait ServiceIndex {
  def getService(id: QualifiedIdentifier): Option[ServiceIndexEntry]
  def requireServices(ids: List[QualifiedIdentifier]): List[ServiceIndexEntry]
  def serviceIds: Set[QualifiedIdentifier]
}

object ServiceIndex {

  def fromServices(
    services: List[DynamicSchemaIndex.ServiceWrapper]
  ): ServiceIndex = {

    val serviceOps: Map[QualifiedIdentifier, Set[OperationName[Id]]] =
      services.map { svc =>
        QualifiedIdentifier.forService(svc.service) ->
          svc
            .service
            .endpoints
            .map(_.name)
            .map(OperationName[Id](_))
            .toSet
      }.toMap

    fromServiceOperationMappings(serviceOps)
  }

  private[playground] def fromServiceOperationMappings(
    mappings: Map[QualifiedIdentifier, Set[OperationName[Id]]]
  ): ServiceIndex =
    new ServiceIndex {

      private val entries = mappings.map { case (id, ops) =>
        id -> (ServiceIndexEntry.Entry(id, ops): ServiceIndexEntry)
      }

      val serviceIds: Set[QualifiedIdentifier] = mappings.keySet

      def getService(
        id: QualifiedIdentifier
      ): Option[ServiceIndexEntry] = entries.get(id)

      def requireServices(
        ids: List[QualifiedIdentifier]
      ): List[ServiceIndexEntry] = ids.map(entries)

    }

}

trait ServiceIndexEntry {
  def id: QualifiedIdentifier
  def operationNames: Set[OperationName[Id]]
  def hasOperation(op: OperationName[Id]): Boolean
}

object ServiceIndexEntry {

  private[playground] case class Entry(id: QualifiedIdentifier, ops: Set[OperationName[Id]])
    extends ServiceIndexEntry {
    def operationNames: Set[OperationName[Id]] = ops

    def hasOperation(op: OperationName[Id]): Boolean = ops.contains_(op)
  }

}
