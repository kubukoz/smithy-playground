package playground.smithyql

import cats.kernel.Monoid
import cats.syntax.all.*
import org.polyvariant.treesitter4s.Node
import util.chaining.*

trait RangeIndex {

  def findAtPosition(
    pos: Position
  ): Option[NodeContext]

}

object RangeIndex {

  given Monoid[RangeIndex] = Monoid.instance(
    _ => None,
    (a, b) => pos => a.findAtPosition(pos).orElse(b.findAtPosition(pos)),
  )

  def build(parsed: playground.generated.nodes.SourceFile): RangeIndex = fromRanges {

    extension (node: Node) {
      def range: SourceRange = SourceRange(Position(node.startByte), Position(node.endByte))
    }

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
      : List[ContextRange] =
      node match {
        case playground.generated.nodes.String_(string) =>
          ContextRange(string.range.shrink1, base.inQuotes) :: Nil

        case playground.generated.nodes.List_(list) =>
          ContextRange(list.range.shrink1, base.inCollectionEntry(None)) ::
            list.list_fields.zipWithIndex.flatMap { (inputNode, i) =>
              ContextRange(inputNode.range, base.inCollectionEntry(Some(i))) ::
                inputNodeRanges(inputNode, base.inCollectionEntry(Some(i)))
            }

        case playground.generated.nodes.Struct(struct) =>
          ContextRange(struct.range, base) ::
            ContextRange(struct.range.shrink1, base.inStructBody) ::
            struct.bindings.toList.flatMap { binding =>
              (binding.key, binding.value).tupled.toList.flatMap { (key, value) =>
                ContextRange(
                  value.range,
                  base
                    .inStructBody
                    .inStructValue(key.source),
                ) :: inputNodeRanges(value, base.inStructBody.inStructValue(key.source))
              }
            }

        case _ => Nil
      }

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
              playground.generated.nodes.InputNode(input).toOption.get /* todo */,
              root.inQuery(statementIndex).inOperationInput,
            )

          }
      }

    }

    preludeRanges ++ queryRanges
  }

  def build(
    sf: SourceFile[WithSource]
  ): RangeIndex = fromRanges {
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

    // Console
    //   .err
    //   .println(
    //     s"""Found ${allRanges.size} ranges for query ${q.operationName.value.text}:
    //        |${allRanges
    //         .map(_.render)
    //         .mkString("\n")}""".stripMargin
    //   )
  }

  def fromRanges(allRanges: List[ContextRange]): RangeIndex =
    pos =>
      allRanges
        .filter(_.range.contains(pos))
        .tap { ranges =>
          println()
          println("=======")
          println(s"all ranges: ${allRanges.map(_.render).mkString(", ")}")
          println(s"ranges for position ${pos.index}: ${ranges.map(_.render).mkString(", ")}")
          println("=======")
          println()
        }
        .maxByOption(_.ctx.length)
        .map(_.ctx)

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
        string = { _ =>
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
  ): List[ContextRange] = {
    // Struct fields that allow nesting in them
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
