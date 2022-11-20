package playground.smithyql

import cats.data.Chain
import cats.data.Chain.==:
import cats.implicits._

// The path to a position in the parsed source
sealed trait NodeContext extends Product with Serializable with NodeContext.PathEntry.TraversalOps {

  def render: String =
    this match {
      case NodeContext.Impl(context) =>
        import NodeContext.PathEntry._

        context
          .map {
            case AtPrelude          => ".prelude"
            case InQuery(index)     => s".query($index)"
            case AtUseClause(index) => s".useClause($index)"
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

    /** Extractor for path entry prefixes. You can use this to assert on a prefix of a path (when
      * you consider paths L->R). For example, in a file like "hello {}", the path at "hello" would
      * be something like
      *
      * {{{EmptyPath.inQuery(0).inOperationName}}}
      *
      * which could be matched with the following pattern:
      *
      * {{{case InQuery(0) ^^: InOperationName ^^: EmptyPath}}}
      *
      * or, if you only want to match on a prefix and keep the rest up for more matching:
      *
      * {{{case InQuery(0) ^^: rest}}}
      */
    def unapply(items: NodeContext): Option[(PathEntry, NodeContext)] = items.uncons
  }

  val EmptyPath: NodeContext = Impl(Chain.nil)

  // This is a bad name really, what it actually means is "empty path" or "no more path" (e.g. in extractors).
  // refactor usages to refer to EmptyPath
  // https://github.com/kubukoz/smithy-playground/issues/159
  val Root: NodeContext = EmptyPath

  private final case class Impl(context: Chain[PathEntry]) extends NodeContext

  sealed trait PathEntry extends Product with Serializable

  object PathEntry {
    case object AtPrelude extends PathEntry
    case class InQuery(index: Int) extends PathEntry
    final case class AtUseClause(index: Int) extends PathEntry
    case object AtOperationName extends PathEntry
    case object AtOperationInput extends PathEntry
    final case class StructValue(key: String) extends PathEntry
    // no index if it's not in an entry - todo replace with CollectionBody?
    final case class CollectionEntry(index: Option[Int]) extends PathEntry
    case object StructBody extends PathEntry
    case object Quotes extends PathEntry

    trait TraversalOps {
      self: NodeContext =>

      def inQuery(index: Int): NodeContext = append(PathEntry.InQuery(index))
      def inPrelude: NodeContext = append(PathEntry.AtPrelude)
      def inUseClause(index: Int): NodeContext = append(PathEntry.AtUseClause(index))
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
