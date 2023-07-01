package playground.lsp.util

import cats.implicits._

case class SerializedSmithyModel(
  smithy: String
)

object SerializedSmithyModel {
  import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
  import com.github.plokhotnyuk.jsoniter_scala.core._
  import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

  private implicit val c: JsonValueCodec[SerializedSmithyModel] = JsonCodecMaker.make

  val decode: Array[Byte] => Either[Throwable, SerializedSmithyModel] =
    bytes =>
      Either
        .catchNonFatal(readFromArray[SerializedSmithyModel](bytes))

}
