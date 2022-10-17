package playground.language

import fs2.io.file.Path
import java.net.URI
import java.nio.file.Paths

final case class Uri(value: String) extends AnyVal {
  def toPath: Path = Path.fromNioPath(Paths.get(new URI(value)))
}

object Uri {
  def fromPath(path: Path): Uri = Uri(path.toNioPath.toUri.toString)
}
