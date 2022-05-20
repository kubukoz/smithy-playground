namespace playground

structure BuildConfig {
  mavenDependencies: Strings,
  mavenRepositories: Strings,
  imports: Strings,
}

list Strings { member: String }
