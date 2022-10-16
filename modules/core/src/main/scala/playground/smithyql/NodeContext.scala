package playground.smithyql

import cats.implicits._
import cats.data.Chain
import cats.data.Chain.==:

// The path to a position in the parsed source
sealed trait NodeContext extends Product with Serializable with NodeContext.PathEntry.TraversalOps {

  def render: String =
    this match {
      case NodeContext.Impl(context) =>
        import NodeContext.PathEntry._

        context
          .map {
            case AtUseClause        => ".useClause"
            case AtOperationName    => ".operationName"
            case AtOperationInput   => ".input"
            case CollectionEntry(i) => s".[${i.getOrElse("")}]"
            case StructValue(key)   => s".$key"
            case StructBody         => ".{}"
            case Quotes             => ".\"\""
          }
          .mkString_("")

    }

  def append(elem: NodeContext.PathEntry): NodeContext =
    this match {
      case NodeContext.Impl(context) => NodeContext.Impl(context.append(elem))
    }

  def length: Long =
    this match {
      case NodeContext.Impl(context) => context.size
    }

  def toList: List[NodeContext.PathEntry] =
    this match {
      case NodeContext.Impl(ctx) => ctx.toList
    }

  def uncons: Option[(NodeContext.PathEntry, NodeContext)] =
    this match {
      case NodeContext.Impl(h ==: t) => Some((h, NodeContext.Impl(t)))
      case _                         => None
    }

  def ^^:(item: NodeContext.PathEntry): NodeContext =
    this match {
      case NodeContext.Impl(context) => NodeContext.Impl(context.prepend(item))
    }

}

object NodeContext {

  object ^^: {
    def unapply(items: NodeContext): Option[(PathEntry, NodeContext)] = items.uncons
  }

  val Root: NodeContext = Impl(Chain.nil)

  private final case class Impl(context: Chain[PathEntry]) extends NodeContext

  sealed trait PathEntry extends Product with Serializable

  object PathEntry {
    case object AtUseClause extends PathEntry
    case object AtOperationName extends PathEntry
    case object AtOperationInput extends PathEntry
    final case class StructValue(key: String) extends PathEntry
    // no index if it's not in an entry - todo replace with CollectionBody?
    final case class CollectionEntry(index: Option[Int]) extends PathEntry
    case object StructBody extends PathEntry
    case object Quotes extends PathEntry

    trait TraversalOps {
      self: NodeContext =>

      def inUseClause: NodeContext = append(PathEntry.AtUseClause)
      def inOperationName: NodeContext = append(PathEntry.AtOperationName)
      def inOperationInput: NodeContext = append(PathEntry.AtOperationInput)
      def inStructValue(key: String): NodeContext = append(PathEntry.StructValue(key))

      def inCollectionEntry(index: Option[Int]): NodeContext = append(
        PathEntry.CollectionEntry(index)
      )

      def inStructBody: NodeContext = append(PathEntry.StructBody)
      def inQuotes: NodeContext = append(PathEntry.Quotes)

    }

  }

}
