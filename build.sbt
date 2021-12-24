import scala.sys.process._

def crossPlugin(x: sbt.librarymanagement.ModuleID) = compilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins = List(
  crossPlugin("org.polyvariant" % "better-tostring" % "0.3.11"),
  crossPlugin("org.typelevel" % "kind-projector" % "0.13.2"),
)

ThisBuild / versionScheme := Some("early-semver")

Global / onChangedBuildSource := ReloadOnSourceChanges

val commonScalaVersions = Seq("2.13.7")

val commonSettings = Seq(
  libraryDependencies ++= compilerPlugins,
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions ++= Seq("source:3.0"),
  Compile / doc / sources := Seq(),
)

lazy val core = projectMatrix
  .settings(
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value,
      "org.http4s" %%% "http4s-ember-client" % "0.23.7",
      "org.typelevel" %%% "cats-parse" % "0.3.6",
      "org.typelevel" %%% "paiges-cats" % "0.4.2",
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
    ),
    commonSettings,
  )
  .jvmPlatform(commonScalaVersions)
  .jsPlatform(commonScalaVersions)
  .enablePlugins(Smithy4sCodegenPlugin)

lazy val main = projectMatrix
  .settings(
    libraryDependencies ++= List(
      "org.http4s" %% "http4s-ember-server" % "0.23.7",
      "com.lihaoyi" %% "pprint" % "0.7.1",
    ),
    fork := true,
    commonSettings,
  )
  .jvmPlatform(commonScalaVersions)
  .dependsOn(core)

lazy val vscode = projectMatrix
  .in(file("vscode-extension"))
  .settings(
    crossScalaVersions := commonScalaVersions,
    moduleName := "smithy-playground-vscode",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % "3.3.1",
      "org.http4s" %%% "http4s-ember-client" % "0.23.7",
      "com.disneystreaming.smithy4s" %%% "smithy4s-http4s" % smithy4sVersion.value,
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
    ),
    commonSettings,
  )
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .dependsOn(core)
  .jsPlatform(
    commonScalaVersions,
    Seq(
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
      externalNpm := {
        Process(
          List("yarn", "--cwd", ((ThisBuild / baseDirectory).value / "vscode-extension").toString)
        ).!
        (ThisBuild / baseDirectory).value / "vscode-extension"
      },
      Compile / fastOptJS / artifactPath := (ThisBuild / baseDirectory).value / "vscode-extension" / "out" / "extension.js",
      Compile / fullOptJS / artifactPath := (ThisBuild / baseDirectory).value / "vscode-extension" / "out" / "extension.js",
    ),
  )

lazy val root = project
  .in(file("."))
  .aggregate(List(core, main, vscode).flatMap(_.projectRefs): _*)
