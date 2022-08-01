namespace playground

use smithy4s.meta#refinement

structure BuildConfig {
  mavenDependencies: Strings,
  mavenRepositories: Strings,
  imports: Strings,
  plugins: Plugins
}

structure Plugins {
  @jsonName("smithy-playground")
  smithyPlayground: SmithyPlaygroundPluginConfig
}

structure SmithyPlaygroundPluginConfig {
  extensions: Strings
}

list Strings { member: String }


// temporary - will move back to tests before merge
@refinement(targetType: "java.time.Instant", providerInstance: "playground.InstantProvider.provider")
@trait
structure instant { }
