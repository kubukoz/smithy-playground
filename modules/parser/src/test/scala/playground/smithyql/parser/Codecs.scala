package playground.smithyql.parser

import io.circe.Codec
import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.smithyql.Listed

object Codecs {

  import io.circe.generic.auto._

  implicit val listedWithSourceCodec: Codec[Listed[WithSource]] =
    io.circe.generic.semiauto.deriveCodec

  implicit val queryWithSourceCodec: Codec[Query[WithSource]] =
    io.circe.generic.semiauto.deriveCodec

}
