package playground.smithyql

import cats.Id
import com.softwaremill.diffx.Diff

object Diffs {
  import com.softwaremill.diffx.cats.*
  import com.softwaremill.diffx.generic.auto.*

  given Diff[Position] = Diff.derived

  given Diff[SourceRange] = Diff.derived

  given Diff[Comment] = Diff.derived

  given Diff[QualifiedIdentifier] = Diff.derived

  given [A: Diff]: Diff[WithSource[A]] = Diff.derived

  given Diff[UseClause[WithSource]] = Diff.derived

  given Diff[InputNode[WithSource]] = Diff.derived

  given Diff[Listed[WithSource]] = Diff.derived

  given Diff[NodeKind] = Diff.derived
  given Diff[Identifier] = Diff.derived

  given Diff[Binding[WithSource]] = Diff.derived

  given Diff[Struct.Fields[WithSource]] = Diff.derived

  given Diff[Struct[WithSource]] = Diff.derived

  given Diff[OperationName[WithSource]] = Diff.derived
  given opNameDiffId: Diff[OperationName[Id]] = Diff.derived

  given Diff[QueryOperationName[WithSource]] = Diff.derived

  given Diff[Query[WithSource]] = Diff.derived

  given Diff[Prelude[WithSource]] = Diff.derived

  given Diff[Statement[WithSource]] = Diff.derived

  given Diff[SourceFile[WithSource]] = Diff.derived

}
