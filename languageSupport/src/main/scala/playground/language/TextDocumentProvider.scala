package playground.language

trait TextDocumentProvider[F[_]] {
  def get(uri: String): F[String]
  def getOpt(uri: String): F[Option[String]]
}

object TextDocumentProvider {
  def apply[F[_]](implicit F: TextDocumentProvider[F]): TextDocumentProvider[F] = F
}
