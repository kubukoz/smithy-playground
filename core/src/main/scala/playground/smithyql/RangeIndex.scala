package playground.smithyql

import cats.implicits._

trait RangeIndex {
  def findAtPosition(pos: Position): Option[ContextRange]
}

object RangeIndex {

  def build(q: Query[WithSource]): RangeIndex =
    new RangeIndex {

      private val allRanges: List[ContextRange] =
        findInUseClause(q.useClause) ++
          findInOperationName(q.operationName) ++
          findInNode(q.input, NodeContext.Root.inOperationInput)

      // Console
      //   .err
      //   .println(
      //     s"""Found ${allRanges.size} ranges for query ${q.operationName.value.text}:
      //        |${allRanges
      //         .map(r => r.ctx.render + " -> " + r.range.render)
      //         .mkString("\n")}""".stripMargin
      //   )

      def findAtPosition(
        pos: Position
      ): Option[ContextRange] = allRanges.filter(_.range.contains(pos)).maxByOption(_.ctx.length)

    }

  private def findInUseClause(
    useClauseOpt: WithSource[Option[UseClause[WithSource]]]
  ): List[ContextRange] =
    useClauseOpt
      .value
      .map { useClause =>
        ContextRange(useClause.identifier.range, NodeContext.Root.inUseClause)
      }
      .toList

  private def findInOperationName(
    operationName: WithSource[OperationName]
  ): List[ContextRange] =
    ContextRange(
      operationName.range,
      NodeContext.Root.inOperationName,
    ) :: Nil

  private def findInNode(
    node: WithSource[InputNode[WithSource]],
    ctx: NodeContext,
  ): List[ContextRange] = {
    def entireNode(ctx: NodeContext) = ContextRange(node.range, ctx)

    node.value match {
      case l @ Listed(_) => entireNode(ctx) :: findInList(l, ctx)

      case s @ Struct(_) => entireNode(ctx) :: findInStruct(s, ctx.inStructBody)

      case StringLiteral(_) =>
        val inQuotes = ContextRange(
          node.range.shrink1,
          ctx.inQuotes,
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
    ctx: NodeContext,
  ): List[ContextRange] = {
    val inItems = list
      .values
      .value
      .zipWithIndex
      .flatMap { case (entry, index) => findInNode(entry, ctx.inCollectionEntry(index.some)) }

    val inBody = ContextRange(
      list
        .values
        .range,
      ctx.inCollectionEntry(None),
    )

    inBody :: inItems
  }

  private def findInStruct(
    struct: Struct[WithSource],
    ctx: NodeContext,
  ): List[ContextRange] =
    // Struct fields that allow nesting in them
    {
      val inFields = struct
        .fields
        .value
        .value
        .flatMap { case (k, v) => findInNode(v, ctx.inStructValue(k.value.text)) }

      ContextRange(struct.fields.range, ctx) :: inFields
    }

}

case class ContextRange(range: SourceRange, ctx: NodeContext)
