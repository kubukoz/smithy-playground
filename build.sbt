def crossPlugin(x: sbt.librarymanagement.ModuleID) = addCompilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins = List(
  crossPlugin("org.polyvariant" % "better-tostring" % "0.3.11"),
  crossPlugin("org.typelevel" % "kind-projector" % "0.13.2"),
)

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scalaVersion := "2.13.7"
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v")),
)

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val main = project
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-parse" % "0.3.6",
      "org.typelevel" %% "paiges-cats" % "0.4.2",
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value,
      "org.http4s" %% "http4s-ember-client" % "0.23.7",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.davegurnell" %% "unindent" % "1.7.0",
      "com.lihaoyi" %% "pprint" % "0.7.1",
    ),
    fork := true,
    compilerPlugins,
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions ++= Seq("source:3.0"),
    Compile / doc / sources := Seq(),
  )
  .enablePlugins(Smithy4sCodegenPlugin)

lazy val root = project
  .in(file("."))
  .aggregate(main)
