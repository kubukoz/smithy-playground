package playground

import cats.effect.IO
import com.softwaremill.diffx.Diff
import weaver.Expectations
import weaver.Log
import weaver.SourceLocation
import com.softwaremill.diffx.ShowConfig

object Assertions extends Expectations.Helpers {

  def assertNoDiff[A: Diff](
    actual: A,
    expected: A,
  )(
    implicit loc: SourceLocation,
    log: Log[IO],
  ): IO[Expectations] =
    Diff[A].apply(expected, actual) match {
      case d if d.isIdentical => IO.pure(success)
      case d                  => log.info(d.show()(ShowConfig.dark)).as(failure("Diff failed"))
    }

}
