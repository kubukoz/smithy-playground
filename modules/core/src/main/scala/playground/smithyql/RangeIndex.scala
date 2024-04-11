package playground.smithyql

import cats.syntax.all.*

trait RangeIndex {

  def findAtPosition(
    pos: Position
  ): NodeContext

}

object RangeIndex {

  def build(
    sf: SourceFile[WithSource]
  ): RangeIndex =
    new RangeIndex {

      private val allRanges: List[ContextRange] = {
        val path = NodeContext.EmptyPath

        val preludeRanges: List[ContextRange] = sf
          .prelude
          .useClauses
          .toNel
          .foldMap { useClauses =>
            val newBase = path.inPrelude

            ContextRange(useClauses.map(_.range).reduceLeft(_.fakeUnion(_)), newBase) ::
              sf.prelude
                .useClauses
                .mapWithIndex {
                  (
                    uc,
                    i,
                  ) =>
                    findInUseClause(uc, newBase.inUseClause(i))
                }
                .combineAll
          }

        val queryRanges = sf.queries(WithSource.unwrap).zipWithIndex.flatMap { case (rq, index) =>
          findInQuery(rq.query, path.inQuery(index))
        }

        preludeRanges ++ queryRanges
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
        .getOrElse(NodeContext.EmptyPath)

    }

  private def findInQuery(
    q: WithSource[Query[WithSource]],
    path: NodeContext,
  ) = {
    val qv = q.value

    List(ContextRange(q.range, path)) ++
      findInOperationName(qv.operationName, path.inOperationName) ++
      findInNode(qv.input, path.inOperationInput)
  }

  private def findInUseClause(
    useClause: WithSource[UseClause[WithSource]],
    path: NodeContext,
  ): List[ContextRange] = ContextRange(useClause.value.identifier.range, path) :: Nil

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
    def entireNode(
      ctx: NodeContext
    ) = ContextRange(node.range, ctx)

    val default = Function.const(
      // Default case: can be triggered e.g. inside a string literal
      // which would affect completions of enum values and timestamps.
      entireNode(ctx) :: Nil
    )

    node
      .value
      .fold(
        listed = l => entireNode(ctx) :: findInList(l, ctx),
        struct = s => entireNode(ctx) :: findInStruct(s, ctx.inStructBody),
        string =
          _ => {
            val inQuotes = ContextRange(
              node.range.shrink1,
              ctx.inQuotes,
            )

            inQuotes :: entireNode(ctx) :: Nil
          },
        int = default,
        bool = default,
        nul = default,
      )

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

case class ContextRange(
  range: SourceRange,
  ctx: NodeContext,
) {
  def render: String = ctx.render + " -> " + range.render
}
