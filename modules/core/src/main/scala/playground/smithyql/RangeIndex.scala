package playground.smithyql

import cats.implicits._

trait RangeIndex {
  def findAtPosition(pos: Position): NodeContext
}

object RangeIndex {

  def build(sf: SourceFile[WithSource]): RangeIndex =
    new RangeIndex {

      // todo: add prelude ranges
      private val allRanges: List[ContextRange] = sf.queries.zipWithIndex.flatMap {
        case (rq, index) => findInQuery(rq.query, NodeContext.Root.inQuery(index))
      }

      // Console
      //   .err
      //   .println(
      //     s"""Found ${allRanges.size} ranges for query ${q.operationName.value.text}:
      //        |${allRanges
      //         .map(_.render)
      //         .mkString("\n")}""".stripMargin
      //   )

      def findAtPosition(
        pos: Position
      ): NodeContext = allRanges
        .filter(_.range.contains(pos))
        .maxByOption(_.ctx.length)
        .map(_.ctx)
        // By default, we're on root level
        .getOrElse(NodeContext.Root)

    }

  private def findInQuery(q: WithSource[Query[WithSource]], path: NodeContext) = {
    val qv = q.value

    List(ContextRange(q.range, path)) ++
      findInUseClause(qv.useClause, path.inUseClause) ++
      findInOperationName(qv.operationName, path.inOperationName) ++
      findInNode(qv.input, path.inOperationInput)
  }

  private def findInUseClause(
    useClauseOpt: WithSource[Option[UseClause[WithSource]]],
    path: NodeContext,
  ): List[ContextRange] =
    useClauseOpt
      .value
      .map { useClause =>
        ContextRange(useClause.identifier.range, path.inUseClause)
      }
      .toList

  private def findInOperationName(
    operationName: WithSource[QueryOperationName[WithSource]],
    path: NodeContext,
  ): List[ContextRange] =
    ContextRange(
      operationName.value.operationName.range,
      path,
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
        .flatMap { binding =>
          findInNode(binding.value, ctx.inStructValue(binding.identifier.value.text))
        }

      ContextRange(struct.fields.range, ctx) :: inFields
    }

}

case class ContextRange(range: SourceRange, ctx: NodeContext) {
  def render: String = ctx.render + " -> " + range.render
}
