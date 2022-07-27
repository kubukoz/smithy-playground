package playground

trait TextDocumentManager[F[_]] {
  def put(uri: String, text: String): F[Unit]
  // todo: extract to separate trait? (read-only is the major usecase)
  def get(uri: String): F[String]
  def remove(uri: String): F[Unit]
}

object TextDocumentManager {
  def apply[F[_]](implicit F: TextDocumentManager[F]): TextDocumentManager[F] = F
}
