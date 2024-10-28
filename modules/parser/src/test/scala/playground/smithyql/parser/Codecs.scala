package playground.smithyql.parser

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import playground.smithyql.Binding
import playground.smithyql.Comment
import playground.smithyql.Diffs.given
import playground.smithyql.Identifier
import playground.smithyql.InputNode
import playground.smithyql.Listed
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.Prelude
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.QueryOperationName
import playground.smithyql.SourceFile
import playground.smithyql.SourceRange
import playground.smithyql.Statement
import playground.smithyql.Struct
import playground.smithyql.UseClause
import playground.smithyql.WithSource

object Codecs {

  given Codec[Position] = Codec.AsObject.derived
  given Codec[Identifier] = Codec.AsObject.derived
  given Codec[QualifiedIdentifier] = Codec.AsObject.derived
  given Codec[Comment] = Codec.AsObject.derived
  given Codec[SourceRange] = Codec.AsObject.derived
  given [A: Codec]: Codec[WithSource[A]] = Codec.AsObject.derived

  given Codec[UseClause[WithSource]] = Codec.AsObject.derived

  given Codec[WithSource[InputNode[WithSource]]] = Codec.AsObject.derived

  given Codec[InputNode[WithSource]] = Codec.AsObject.derived

  // strange thing - somehow doesn't get picked up automatically.
  given Codec[List[WithSource[InputNode[WithSource]]]] = Codec.from(
    Decoder.decodeList,
    Encoder.encodeList,
  )

  given Codec[Listed[WithSource]] = Codec.AsObject.derived

  given Codec[Binding[WithSource]] = Codec.AsObject.derived
  given Codec[Struct.Fields[WithSource]] = Codec.AsObject.derived

  given Codec[Struct[WithSource]] = Codec.AsObject.derived
  given Codec[OperationName[WithSource]] = Codec.AsObject.derived
  given Codec[QueryOperationName[WithSource]] = Codec.AsObject.derived
  given Codec[Query[WithSource]] = Codec.AsObject.derived

  given Codec[Prelude[WithSource]] = Codec.AsObject.derived
}
