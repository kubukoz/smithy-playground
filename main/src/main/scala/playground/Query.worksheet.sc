import com.disneystreaming.demo.smithy.DemoServiceGen

import cats.effect.unsafe.implicits._

import playground._
import Formatter._
import SmithyQLParser._

val q = parse {
  """CreateHero { hero = { good = { howGood = 42 }, }, } """
}

import DSL._

val q2 = "CreateHero".call(
  "hero" -> struct("good" -> struct("howGood" -> 42))
)

println(format(q))

Runner
  .make(DemoServiceGen)
  .use {
    _.run(q)
  }
  .unsafeRunSync()
