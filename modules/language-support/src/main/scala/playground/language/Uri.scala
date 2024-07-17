package playground.language

import fs2.io.file.Path

import java.net.URI
import java.nio.file.Paths

// not making constructor private because
// "access modifiers from `copy` method are copied from the case class constructor"
// cannot nicely be silenced in scala 2
final case class Uri /* private */ (
  value: String
) extends AnyVal {
  def toPath: Path = Path.fromNioPath(Paths.get(new URI(value)))

  // :/
  // only for tests!
  def /(
    subdir: String
  ): Uri = Uri(value + "/" + subdir)

}

object Uri {

  def fromPath(
    path: Path
  ): Uri = fromUriString(path.toNioPath.toUri().toString())

  def fromUriString(
    s: String
  ): Uri = new Uri(s)

}
