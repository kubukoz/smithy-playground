package playground.lsp

import cats.effect.IO
import playground.BuildConfig
import weaver._

object PluginResolverTests extends SimpleIOSuite {
  test("Empty plugin resolver finds no plugins") {
    PluginResolver
      .instance[IO]
      .resolve(BuildConfig(mavenDependencies = Nil, mavenRepositories = Nil))
      .map(assert.same(_, Nil))
  }

  test("Plugin resolver with a sample plugin artifact finds it") {
    PluginResolver
      .instance[IO]
      .resolve(
        BuildConfig(
          mavenDependencies = List(
            "com.kubukoz.playground::plugin-sample:latest.integration"
          ),
          mavenRepositories = Nil,
        )
      )
      .map(_.map(_.getClass().getName()))
      .map(assert.same(_, List("playground.sample.SamplePlaygroundPlugin")))
  }
}
