package playground

trait TextDocumentProvider[F[_]] {
  def get(uri: String): F[String]
}

object TextDocumentProvider {
  def apply[F[_]](implicit F: TextDocumentProvider[F]): TextDocumentProvider[F] = F
}
