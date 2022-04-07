package playground

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import demo.smithy.CreateHeroOutput
import demo.smithy.CreateSubscriptionOutput
import demo.smithy.DemoService
import demo.smithy.GetPowersOutput
import demo.smithy.Hero
import demo.smithy.Hero.BadCase
import demo.smithy.HeroIsBad
import demo.smithy.Power
import demo.smithy.Subscription
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import smithy4s.http4s.SimpleRestJsonBuilder
import demo.smithy.GenericServerError

object client {

  def make[F[_]: Async](useNetwork: Boolean): Resource[F, Client[F]] = {
    val fakeClient = SimpleRestJsonBuilder
      .routes {
        new DemoService[F] {
          def createHero(hero: Hero, verbose: Option[Boolean]): F[CreateHeroOutput] =
            hero match {
              case BadCase(bad) if bad.evilName == "die" =>
                GenericServerError("generic error").raiseError[F, CreateHeroOutput]

              case BadCase(bad) if bad.evilName == "fail" =>
                HeroIsBad(bad.powerLevel).raiseError[F, CreateHeroOutput]

              case _ => CreateHeroOutput(hero).pure[F]
            }

          def createSubscription(subscription: Subscription): F[CreateSubscriptionOutput] =
            CreateSubscriptionOutput(subscription).pure[F]

          def getPowers(): F[GetPowersOutput] = GetPowersOutput(List(Power.FIRE, Power.ICE)).pure[F]
        }
      }
      .resource
      .map(_.orNotFound)
      .map(Client.fromHttpApp(_))

    {
      if (useNetwork)
        EmberClientBuilder.default[F].build
      else
        fakeClient
    }
  }

}
