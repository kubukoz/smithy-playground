$version: "2"

namespace playground

structure BuildConfig {
  mavenDependencies: Strings = [],
  mavenRepositories: Strings = [],
  maven: MavenConfig,
  imports: Strings = [],
  smithyPlayground: SmithyPlaygroundPluginConfig
}

structure SmithyPlaygroundPluginConfig {
  extensions: Strings = []
}

list Strings { member: String }

structure MavenConfig {
  dependencies: Strings = [],
  repositories: Repositories = []
}

list Repositories { member: Repository }

structure Repository {
  @required
  url: String
}
