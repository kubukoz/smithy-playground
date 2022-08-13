package playground

import playground.smithyql.InputNode
import playground.smithyql.Listed
import playground.smithyql.OperationName
import playground.smithyql.SmithyQLParser
import playground.smithyql.SourceRange
import playground.smithyql.Struct
import playground.smithyql.UseClause
import playground.smithyql.WithSource

object DocumentSymbolProvider {

  def make(text: String): List[DocumentSymbol] =
    SmithyQLParser.parseFull(text) match {
      case Left(_) => Nil
      case Right(q) =>
        findInUseClause(q.useClause) ++
          findInOperation(q.operationName, q.input)
    }

  private def findInUseClause(
    clause: WithSource[Option[UseClause[WithSource]]]
  ): List[DocumentSymbol] =
    clause
      .value
      .map { useClause =>
        DocumentSymbol(
          useClause.identifier.value.render,
          SymbolKind.Package,
          useClause.identifier.range,
          useClause.identifier.range,
          Nil,
        )
      }
      .toList

  private def findInOperation(
    op: WithSource[OperationName[WithSource]],
    body: WithSource[Struct[WithSource]],
  ): List[DocumentSymbol] =
    DocumentSymbol(
      op.value.text,
      SymbolKind.Function,
      selectionRange = op.range,
      range = op.range.fakeUnion(body.range),
      children = findInStruct(body),
    ) :: Nil

  private def findInStruct(
    struct: WithSource[Struct[WithSource]]
  ): List[DocumentSymbol] = struct.value.fields.value.value.map { case (key, value) =>
    DocumentSymbol(
      key.value.text,
      SymbolKind.Field,
      selectionRange = key.range,
      range = key.range.fakeUnion(value.range),
      children = findInNode(value),
    )
  }

  private def findInNode(node: WithSource[InputNode[WithSource]]): List[DocumentSymbol] = node
    .value
    .fold(
      struct = struct => findInStruct(node.copy(value = struct)),
      string = _ => Nil,
      int = _ => Nil,
      listed = list => findInList(node.copy(value = list)),
      bool = _ => Nil,
    )

  private def findInList(
    list: WithSource[Listed[WithSource]]
  ): List[DocumentSymbol] = list.value.values.value.zipWithIndex.map { case (item, i) =>
    DocumentSymbol(
      i.toString(),
      SymbolKind.Array,
      item.range,
      item.range,
      findInNode(item),
    )
  }

}

final case class DocumentSymbol(
  name: String,
  kind: SymbolKind,
  selectionRange: SourceRange,
  range: SourceRange,
  children: List[DocumentSymbol],
)

sealed trait SymbolKind extends Product with Serializable

object SymbolKind {
  case object Function extends SymbolKind
  case object Package extends SymbolKind
  case object Array extends SymbolKind
  case object Field extends SymbolKind
}
