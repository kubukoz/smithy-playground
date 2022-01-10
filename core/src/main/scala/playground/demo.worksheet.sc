import demo.smithy.Good

import playground.smithyql.OperationName

import playground.smithyql.Position

import cats.Id

import playground.smithyql.Query
import playground.smithyql.Formatter
import demo.smithy.Hero

import demo.smithy.CreateSubscriptionOutput
import demo.smithy.Subscription
import demo.smithy.DemoService
import cats.effect.IO

import demo.smithy.DemoServiceOperation

import java.security.interfaces.DSAKeyPairGenerator
import demo.smithy.DemoServiceGen

import demo.smithy.CreateHeroOutput

import playground.NodeEncoderSchematic
import playground.smithyql.SourceRange
import cats.effect.unsafe.implicits._
import playground.smithyql.WithSource

import playground.smithyql.Struct

import playground.smithyql.SmithyQLParser

val raw = """
CreateHero {
  hero = {
    bad = {
      evilName = "Devil",
      powerLevel = 420,
    },
  },
}
"""

val q =
  SmithyQLParser
    .parseFull(raw)
    .toTry
    .get

def show(range: SourceRange) = raw.substring(range.start.index, range.end.index)

val result = playground
  .Compiler
  .instance[DemoServiceGen, DemoServiceOperation, IO](DemoServiceGen)

val input = result.compile(SmithyQLParser.parseFull(raw).toOption.get).unsafeRunSync()

val op = input.endpoint.wrap(input.input)

val ds =
  new DemoService[IO] {

    def createHero(hero: Hero): IO[CreateHeroOutput] = IO(
      CreateHeroOutput(Hero.GoodCase(Good(420)))
    )

    def createSubscription(subscription: Subscription): IO[CreateSubscriptionOutput] = IO(
      CreateSubscriptionOutput(subscription)
    )

  }

val out: CreateHeroOutput = DemoServiceGen
  .asTransformation(ds)
  .apply(op)
  .unsafeRunSync()
  .asInstanceOf[CreateHeroOutput]

//
//
//

import cats.~>

val wrapWithSource: Id ~> WithSource =
  new (Id ~> WithSource) {

    def apply[A](
      fa: Id[A]
    ): WithSource[A] = WithSource(Nil, Nil, SourceRange(Position(0), Position(0)), fa)

  }

// Formatter
//   .writeAst(
//     CreateHeroOutput
//       .schema
//       .compile(NodeEncoderSchematic)
//       .toNode(out)
//       .mapK(wrapWithSource)
//   )
//   .renderTrim(80)
