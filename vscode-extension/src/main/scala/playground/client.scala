package playground

import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.implicits._
import demo.smithy.CreateHeroOutput
import demo.smithy.CreateSubscriptionOutput
import demo.smithy.DemoService
import demo.smithy.GetPowersOutput
import demo.smithy.Hero
import demo.smithy.Power
import demo.smithy.Subscription
import fs2.io.net.Network
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import smithy4s.http4s.SimpleRestJsonBuilder

object client {

  def make[F[_]: Async](useNetwork: Boolean): Resource[F, Client[F]] = {
    val fakeClient = SimpleRestJsonBuilder
      .routes {
        new DemoService[F] {
          def createHero(
            hero: Hero,
            verbose: Option[Boolean],
          ): F[CreateHeroOutput] = CreateHeroOutput(hero).pure[F]

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
        Network[F].tlsContext.system.toResource.flatMap { tls =>
          EmberClientBuilder.default[F].withTLSContext(tls).build
        }
      else
        fakeClient
    }
  }

}
