import scala.sys.process._

def crossPlugin(x: sbt.librarymanagement.ModuleID) = compilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins = List(
  crossPlugin("org.polyvariant" % "better-tostring" % "0.3.15")
)

ThisBuild / versionScheme := Some("early-semver")

Global / onChangedBuildSource := ReloadOnSourceChanges

val commonScalaVersions = Seq("3.1.1")

val commonSettings = Seq(
  organization := "com.kubukoz.playground",
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-effect" % "3.3.13",
    "com.disneystreaming" %%% "weaver-cats" % "0.7.13" % Test,
    "com.disneystreaming" %%% "weaver-discipline" % "0.7.13" % Test,
    "com.disneystreaming" %%% "weaver-scalacheck" % "0.7.13" % Test,
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  libraryDependencies ++= compilerPlugins,
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions -= "-Vtype-diffs",
  scalacOptions += "-scalajs",
)

lazy val core = projectMatrix
  .settings(
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %%% "smithy4s-dynamic" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %%% "smithy4s-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %%% "smithy4s-aws-http4s" % smithy4sVersion.value,
      "org.typelevel" %%% "cats-parse" % "0.3.7",
      "org.typelevel" %%% "paiges-cats" % "0.4.2",
      "com.lihaoyi" %%% "sourcecode" % "0.2.8",
    ),
    commonSettings,
    buildInfoPackage := "playground.buildinfo",
    buildInfoKeys ++= Seq(
      smithy4sVersion
    ),
  )
  .jvmPlatform(commonScalaVersions)
  .jsPlatform(commonScalaVersions)
  .enablePlugins(Smithy4sCodegenPlugin)
  .enablePlugins(BuildInfoPlugin)

lazy val vscode = projectMatrix
  .in(file("vscode-extension"))
  .settings(
    crossScalaVersions := commonScalaVersions,
    moduleName := "smithy-playground-vscode",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-ember-client" % "0.23.13"
    ),
    commonSettings,
  )
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .dependsOn(core)
  .jsPlatform(
    commonScalaVersions,
    Seq(
      externalNpm := {
        Process(
          List("yarn", "--cwd", ((ThisBuild / baseDirectory).value / "vscode-extension").toString)
        ).!
        (ThisBuild / baseDirectory).value / "vscode-extension"
      },
      Compile / fastOptJS / artifactPath := (ThisBuild / baseDirectory).value / "vscode-extension" / "out" / "extension.js",
      Compile / fullOptJS / artifactPath := (ThisBuild / baseDirectory).value / "vscode-extension" / "out" / "extension.js",
      test := {},
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    ),
  )

lazy val cli = projectMatrix
  .in(file("cli"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-ember-client" % "0.23.13",
      "com.monovore" %% "decline-effect" % "2.3.0",
      "com.disneystreaming.smithy4s" %% "smithy4s-codegen-cli" % smithy4sVersion.value,
    ),
  )
  .dependsOn(core)
  .jvmPlatform(commonScalaVersions)

lazy val root = project
  .in(file("."))
  .aggregate(List(core, vscode, cli).flatMap(_.projectRefs): _*)
