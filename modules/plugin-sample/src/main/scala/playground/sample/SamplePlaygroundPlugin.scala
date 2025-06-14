package playground.sample

import cats.effect.kernel.Async
import playground.plugins.Environment
import playground.plugins.Interpreter
import playground.plugins.PlaygroundPlugin

class SamplePlaygroundPlugin extends PlaygroundPlugin {
  def interpreters[F[_]: Environment: Async]: List[Interpreter[F]] = Nil
}
