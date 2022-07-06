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
import demo.smithy.Hero.BadCase
import demo.smithy.HeroIsBad
import demo.smithy.Power
import demo.smithy.Subscription
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import smithy4s.http4s.SimpleRestJsonBuilder
import demo.smithy.GenericServerError
import fs2.io.net.tls.TLSContext
import fs2.io.net.tls.SecureContext
import org.http4s.client.middleware.Logger
import typings.vscode.mod.OutputChannel
import cats.effect.kernel.Sync

object client {

  def make[F[_]: Async](
    useNetwork: Boolean,
    chan: OutputChannel,
  ): Resource[F, Client[F]] = {
    val fakeClient = SimpleRestJsonBuilder
      .routes {
        new DemoService[F] {
          def createHero(
            hero: Hero,
            verbose: Option[Boolean],
            powers: Option[List[Power]],
          ): F[CreateHeroOutput] =
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
        Async[F]
          .delay(
            // todo: use facade
            TLSContext
              .Builder
              .forAsync[F]
              .fromSecureContext(
                SecureContext
                  .fromJS(
                    scalajs
                      .js
                      .Dynamic
                      .global
                      .require("tls")
                      .applyDynamic("createSecureContext")(
                        scalajs
                          .js
                          .Object
                          .fromEntries(
                            scalajs
                              .js
                              .Array(
                                scalajs
                                  .js
                                  .Tuple2(
                                    "_vscodeAdditionalCaCerts",
                                    scalajs
                                      .js
                                      .Array
                                      .apply(),
                                  )
                              )
                          )
                      )
                  )
              )
          )
          .toResource
          .flatMap { tls =>
            EmberClientBuilder.default[F].withTLSContext(tls).build
          }
          .map(
            Logger[F](
              logHeaders = true,
              logBody = true,
              logAction = Some(s => Sync[F].delay(chan.appendLine(s))),
            )
          )
      else
        fakeClient
    }
  }

}
