import org.typelevel.paiges.Doc

import com.disneystreaming.demo.smithy.DemoServiceGen

import cats.effect.unsafe.implicits._

import playground._
import Formatter._
import SmithyQLParser._

val q = parse(
  """
CreateHero {
  hero = {
  // bad = {
  //   evilName = "evil",
  //   powerLevel = 9001,
  // },
    good = {
      howGood = 200,
      anotherKey = 42,
    },
  },
}
"""
)

{
  import DSL._

  val q2 = "CreateHero".call(
    "hero" -> struct("good" -> struct("howGood" -> 42))
  )

  format(q, 10)
  format(q, 20)
  format(q, 40)
  format(q, 100)
}

Runner
  .make(DemoServiceGen)
  .use {
    _.run(q)
  }
  .unsafeRunSync()
