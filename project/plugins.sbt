ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.8.7")
addSbtPlugin("org.typelevel" % "sbt-typelevel-mergify" % "0.8.7")

// explicit bump: 3.0.0 supports git worktrees
addSbtPlugin("com.github.sbt" % "sbt-git" % "3.0.0")

addSbtPlugin("com.disneystreaming.smithy4s" % "smithy4s-sbt-codegen" % "0.18.41")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")

addDependencyTreePlugin
