package playground.language

trait TextDocumentProvider[F[_]] {
  def get(uri: Uri): F[String]
  def getOpt(uri: Uri): F[Option[String]]
}

object TextDocumentProvider {
  def apply[F[_]](implicit F: TextDocumentProvider[F]): TextDocumentProvider[F] = F
}
