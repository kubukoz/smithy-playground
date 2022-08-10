package playground.smithyql

import cats.implicits._

trait RangeIndex {
  def findAtPosition(pos: Position): Option[ContextRange]
}

object RangeIndex {

  def build(q: Query[WithSource]): RangeIndex =
    new RangeIndex {

      private val allRanges: List[ContextRange] =
        findInOperationName(q.operationName) ++ findInNode(q.input, NodeContext.InputContext.root)

      def findAtPosition(
        pos: Position
      ): Option[ContextRange] = allRanges.filter(_.range.contains(pos)).maxByOption(_.ctx.length)

    }

  private def findInOperationName(
    operationName: WithSource[OperationName]
  ): List[ContextRange] =
    ContextRange(operationName.range, NodeContext.OperationContext(operationName)) :: Nil

  private def findInNode(
    node: WithSource[InputNode[WithSource]],
    ctx: NodeContext.InputContext,
  ): List[ContextRange] = {
    def entireNode(ctx: NodeContext.InputContext) = ContextRange(node.range, ctx)

    node.value match {
      case l @ Listed(_) => entireNode(ctx) :: findInList(l, ctx)

      case s @ Struct(_) =>
        entireNode(ctx) :: findInStruct(s, ctx.append(NodeContext.PathEntry.StructBody))

      case StringLiteral(_) =>
        val inQuotes = ContextRange(
          node.range.shrink1,
          ctx.append(NodeContext.PathEntry.Quotes),
        )

        inQuotes :: entireNode(ctx) :: Nil

      case _ =>
        // Default case: can be triggered e.g. inside a string literal
        // which would affect completions of enum values and timestamps.
        entireNode(ctx) :: Nil
    }

  }

  private def findInList(
    list: Listed[WithSource],
    ctx: NodeContext.InputContext,
  ): List[ContextRange] = {
    val inItems = list
      .values
      .value
      .zipWithIndex
      .flatMap { case (entry, index) =>
        findInNode(entry, ctx.append(NodeContext.PathEntry.CollectionEntry(index.some)))
      }

    val inBody = ContextRange(
      list
        .values
        .range,
      ctx.append(NodeContext.PathEntry.CollectionEntry(None)),
    )

    inBody :: inItems
  }

  private def findInStruct(
    struct: Struct[WithSource],
    ctx: NodeContext.InputContext,
  ): List[ContextRange] =
    // Struct fields that allow nesting in them
    {
      val inFields = struct
        .fields
        .value
        .value
        .flatMap { case (k, v) =>
          findInNode(v, ctx.append(NodeContext.PathEntry.StructValue(k.value.text)))
        }

      ContextRange(struct.fields.range, ctx) :: inFields
    }

}

case class ContextRange(range: SourceRange, ctx: NodeContext)
