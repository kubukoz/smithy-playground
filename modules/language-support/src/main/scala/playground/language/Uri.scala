package playground.language

import fs2.io.file.Path

import java.nio.file.Paths
import java.net.URI

final case class Uri private (value: String) extends AnyVal {
  def toPath: Path = Path.fromNioPath(Paths.get(new URI(value)))

  // :/
  // only for tests!
  def /(subdir: String): Uri = Uri(value + "/" + subdir)
}

object Uri {
  def fromPath(path: Path): Uri = fromUriString(path.toNioPath.toUri().toString())
  def fromUriString(s: String): Uri = new Uri(s)
}
