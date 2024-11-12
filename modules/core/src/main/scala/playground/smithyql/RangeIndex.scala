package playground.smithyql

import cats.syntax.all.*
import tsutils.*
import util.chaining.*

trait RangeIndex {

  def findAtPosition(
    pos: Position
  ): Option[NodeContext]

}

object RangeIndex {

  def build(parsed: playground.generated.nodes.SourceFile): RangeIndex = fromRanges {

    val root = NodeContext.EmptyPath

    val preludeRanges = parsed
      .prelude
      .toList
      .flatMap { prelude =>
        val newBase = root.inPrelude
        ContextRange(prelude.range, newBase) ::
          prelude.use_clause.zipWithIndex.map { (useClause, i) =>
            ContextRange(useClause.range, newBase.inUseClause(i))
          }
      }

    def inputNodeRanges(node: playground.generated.nodes.InputNode, base: NodeContext)
      : List[ContextRange] = node.visit(
      new playground.generated.nodes.InputNode.Visitor.Default[List[ContextRange]] {
        def default: List[ContextRange] = Nil

        override def onString(node: playground.generated.nodes.String_): List[ContextRange] =
          ContextRange(node.range.shrink1, base.inQuotes) :: Nil

        override def onList(node: playground.generated.nodes.List_): List[ContextRange] =
          ContextRange(node.range.shrink1, base.inCollectionEntry(None)) ::
            node.list_fields.zipWithIndex.flatMap { (inputNode, i) =>
              ContextRange(inputNode.range, base.inCollectionEntry(Some(i))) ::
                inputNodeRanges(inputNode, base.inCollectionEntry(Some(i)))
            }

        override def onStruct(node: playground.generated.nodes.Struct): List[ContextRange] =
          ContextRange(node.range, base) ::
            ContextRange(node.range.shrink1, base.inStructBody) ::
            node.bindings.toList.flatMap { binding =>
              (binding.key, binding.value).tupled.toList.flatMap { (key, value) =>
                ContextRange(value.range, base.inStructBody.inStructValue(key.source)) ::
                  inputNodeRanges(value, base.inStructBody.inStructValue(key.source))
              }
            }
      }
    )

    val queryRanges = parsed.statements.zipWithIndex.flatMap { (stat, statementIndex) =>
      stat.run_query.toList.flatMap { runQuery =>
        ContextRange(runQuery.range, root.inQuery(statementIndex)) :: runQuery
          .operation_name
          .toList
          .flatMap { operationName =>
            ContextRange(operationName.range, root.inQuery(statementIndex).inOperationName) :: Nil
          } ++
          runQuery.input.toList.flatMap { input =>
            inputNodeRanges(
              playground.generated.nodes.InputNode(input),
              root.inQuery(statementIndex).inOperationInput,
            )

          }
      }

    }

    preludeRanges ++ queryRanges
  }

  def fromRanges(allRanges: List[ContextRange]): RangeIndex =
    pos =>
      allRanges
        .filter(_.range.contains(pos))
        .tap { ranges =>
          // println()
          // println("=======")
          // println(s"all ranges: ${allRanges.map(_.render).mkString(", ")}")
          // println(s"ranges for position ${pos.index}: ${ranges.map(_.render).mkString(", ")}")
          // println("=======")
          // println()
        }
        .maxByOption(_.ctx.length)
        .map(_.ctx)

}

case class ContextRange(
  range: SourceRange,
  ctx: NodeContext,
) {
  def render: String = ctx.render + " -> " + range.render
}
