package playground

trait TextDocumentManager[F[_]] {
  def put(uri: String, text: String): F[Unit]
  def get(uri: String): F[String]
  def remove(uri: String): F[Unit]
}

object TextDocumentManager {
  def apply[F[_]](implicit F: TextDocumentManager[F]): TextDocumentManager[F] = F
}
