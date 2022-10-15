package playground.smithyql.parser

import io.circe.Codec
import playground.smithyql.Query
import playground.smithyql.WithSource

object Codecs {

  implicit val queryWithSourceCodec: Codec[Query[WithSource]] = {
    import io.circe.generic.auto._

    io.circe.generic.semiauto.deriveCodec
  }

}
