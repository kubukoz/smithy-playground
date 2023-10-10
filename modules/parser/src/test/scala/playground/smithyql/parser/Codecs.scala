package playground.smithyql.parser

import io.circe.Codec
import playground.smithyql.Binding
import playground.smithyql.Comment
import playground.smithyql.InputNode
import playground.smithyql.Listed
import playground.smithyql.Prelude
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.SourceFile
import playground.smithyql.SourceRange
import playground.smithyql.Statement
import playground.smithyql.Struct
import playground.smithyql.UseClause
import playground.smithyql.WithSource

object Codecs {

  given Codec[QualifiedIdentifier] = Codec.AsObject.derived
  given Codec[Comment] = Codec.AsObject.derived
  given Codec[SourceRange] = Codec.AsObject.derived
  given [A: Codec]: Codec[WithSource[A]] = Codec.AsObject.derived

  given Codec[UseClause[WithSource]] = Codec.AsObject.derived

  given Codec[InputNode[WithSource]] = Codec.AsObject.derived
  given Codec[Listed[WithSource]] = Codec.AsObject.derived

  given Codec[Binding[WithSource]] = Codec.AsObject.derived
  given Codec[Struct.Fields[WithSource]] = Codec.AsObject.derived
  given Codec[Struct[WithSource]] = Codec.AsObject.derived

  given Codec[Query[WithSource]] = Codec.AsObject.derived

  given Codec[Prelude[WithSource]] = Codec.AsObject.derived

  given Codec[Statement[WithSource]] = Codec.AsObject.derived
  given Codec[SourceFile[WithSource]] = Codec.AsObject.derived
}
