package playground.smithyql.parser

import io.circe.Codec
import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.smithyql.Listed
import playground.smithyql.Struct
import playground.smithyql.UseClause

object Codecs {

  import io.circe.generic.auto._
  import io.circe.generic.semiauto._

  implicit val useClauseWithSourceCodec: Codec[UseClause[WithSource]] = deriveCodec

  implicit val listedWithSourceCodec: Codec[Listed[WithSource]] = deriveCodec

  implicit val structWithSourceCodec: Codec[Struct[WithSource]] = deriveCodec

  implicit val queryWithSourceCodec: Codec[Query[WithSource]] = deriveCodec

}
