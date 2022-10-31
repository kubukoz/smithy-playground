package playground.lsp

import io.circe.Codec
import io.circe.Encoder
import io.circe.Decoder
import org.http4s.Uri
import cats.implicits._
import io.circe.Json

trait ConfigurationValue[T] {
  def key: String
  def codec: Codec[T]
  def apply(value: T): ConfigurationValue.Applied[T] = ConfigurationValue.Applied(this, value)
}

object ConfigurationValue {

  def make[A: Encoder: Decoder](k: String): ConfigurationValue[A] =
    new ConfigurationValue[A] {
      val key: String = k
      val codec: Codec[A] = Codec.from(implicitly, implicitly)
    }

  final case class Applied[T](cv: ConfigurationValue[T], value: T) {
    def encoded: Json = cv.codec.apply(value)
  }

  implicit val uriJsonDecoder: Decoder[Uri] = Decoder[String].emap(
    Uri.fromString(_).leftMap(_.message)
  )

  implicit val uriJsonEncoder: Encoder[Uri] = Encoder[String].contramap(_.renderString)

  val maxWidth: ConfigurationValue[Int] = make[Int]("smithyql.formatter.maxWidth")
  val baseUri: ConfigurationValue[Uri] = make[Uri]("smithyql.http.baseUrl")

  val authorizationHeader: ConfigurationValue[String] = make[String](
    "smithyql.http.authorizationHeader"
  )

}
