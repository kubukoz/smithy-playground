def crossPlugin(x: sbt.librarymanagement.ModuleID) = addCompilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins = List(
  crossPlugin("org.typelevel" % "kind-projector" % "0.13.2"),
  crossPlugin("org.polyvariant" % "better-tostring" % "0.3.11"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
)

ThisBuild / scalaVersion := "2.13.7"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val main = project
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-parse" % "0.3.6",
      "org.typelevel" %% "paiges-cats" % "0.4.2",
      "com.disneystreaming.oss" %% "smithy4s-http4s" % "0.8.1-52-f46ae9c0",
    ),
    fork := true,
    compilerPlugins,
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions ++= Seq("source:3.0"),
    Compile / doc / sources := Seq(),
  )

lazy val root = project
  .in(file("."))
  .aggregate(main)
