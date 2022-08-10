package playground.smithyql

import cats.implicits._
import cats.data.Chain

// The path to a position in the parsed source
sealed trait NodeContext extends Product with Serializable {
  def render: String

  def length: Long =
    this match {
      case NodeContext.OperationContext(_)   => 1
      case NodeContext.InputContext(context) => 1 + context.size
    }

}

object NodeContext {

  final case class OperationContext(opName: WithSource[OperationName]) extends NodeContext {
    def render: String = ".operationName"
  }

  final case class InputContext(context: Chain[PathEntry]) extends NodeContext {
    def append(elem: PathEntry) = copy(context.append(elem))

    def toList = context.toList

    def render: String = {
      import PathEntry._
      context
        .map {
          case CollectionEntry(i) => s".[${i.getOrElse("")}]"
          case StructValue(key)   => s".$key"
          case StructBody         => ".{}"
          case Quotes             => ".\"\""
        }
        .mkString_(".input", "", "")
    }

  }

  object InputContext {
    val root: InputContext = InputContext(Chain.nil)
  }

  sealed trait PathEntry extends Product with Serializable

  object PathEntry {
    final case class StructValue(key: String) extends PathEntry
    // no index if it's not in an entry - todo replace with CollectionBody?
    final case class CollectionEntry(index: Option[Int]) extends PathEntry
    case object StructBody extends PathEntry
    case object Quotes extends PathEntry
  }

}
