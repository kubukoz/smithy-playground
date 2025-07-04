inThisBuild(
  List(
    organization := "com.kubukoz.playground",
    homepage := Some(url("https://github.com/kubukoz/smithy-playground")),
    licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "kubukoz",
        "Jakub Kozłowski",
        "kubukoz@gmail.com",
        url("https://kubukoz.com"),
      )
    ),
    tlBaseVersion := "0.10",
  )
)

val ScalaLTS = "3.3.6"
val ScalaNext = "3.7.1"

val jsoniterVersion = "2.36.6"

ThisBuild / scalaVersion := ScalaNext
ThisBuild / versionScheme := Some("early-semver")

import scala.sys.process.*
import scala.util.chaining.*

// Workaround for https://github.com/coursier/coursier/issues/2001
// from https://github.com/coursier/coursier/issues/2001#issuecomment-2556556628
def jsoniterFix(deps: Seq[ModuleID]) =
  deps.map(
    _.exclude("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-core_3")
  ) ++ Seq(
    "com.github.plokhotnyuk.jsoniter-scala" % "jsoniter-scala-core_2.13" % jsoniterVersion
  )

def crossPlugin(
  x: sbt.librarymanagement.ModuleID
) = compilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins =
  libraryDependencies ++= List(
    crossPlugin("org.polyvariant" % "better-tostring" % "0.3.17")
  ) ++ (if (scalaVersion.value.startsWith("3"))
          Nil
        else
          List(
            crossPlugin("org.typelevel" % "kind-projector" % "0.13.3")
          ))

val commonSettings = Seq(
  organization := "com.kubukoz.playground",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.13.0",
    "org.typelevel" %% "cats-mtl" % "1.5.0",
    "org.typelevel" %% "weaver-cats" % "0.9.1" % Test,
    "org.typelevel" %% "weaver-discipline" % "0.9.1" % Test,
    "org.typelevel" %% "weaver-scalacheck" % "0.9.1" % Test,
    "com.softwaremill.diffx" %% "diffx-core" % "0.9.0" % Test,
    "com.softwaremill.diffx" %% "diffx-cats" % "0.9.0" % Test,
  ),
  compilerPlugins,
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions -= "-Vtype-diffs",
  scalacOptions -= "-language:existentials",
  // https://github.com/lampepfl/dotty/issues/18674
  Test / scalacOptions -= "-Wunused:implicits",
  Test / scalacOptions -= "-Wunused:explicits",
  Test / scalacOptions -= "-Wunused:imports",
  Test / scalacOptions -= "-Wunused:locals",
  Test / scalacOptions -= "-Wunused:params",
  Test / scalacOptions -= "-Wunused:privates",
  //
  scalacOptions += "-no-indent",
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("3.7"))
      Seq(
        // for cats-tagless macros
        "-experimental"
      )
    else
      Nil
  },
  Test / scalacOptions += "-Wconf:cat=deprecation:silent,msg=Specify both message and version:silent",
  scalacOptions += "-release:11",
  mimaFailOnNoPrevious := false,
  mimaPreviousArtifacts := Set.empty,
)

def module(
  name: String
) = Project(name, file("modules") / name)
  .settings(
    commonSettings
  )

// Plugin interface. Sometimes keeps binary compatibility guarantees (mostly tied to smithy4s's bincompat).
lazy val pluginCore = module("plugin-core").settings(
  libraryDependencies ++= Seq(
    "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value
  ).pipe(jsoniterFix),
  // mimaPreviousArtifacts := Set(organization.value %% name.value % "0.7.0"),
  mimaPreviousArtifacts := Set.empty,
  scalaVersion := ScalaLTS,
)

lazy val pluginSample = module("plugin-sample")
  .dependsOn(pluginCore)
  .settings(
    // used for tests only
    mimaPreviousArtifacts := Set.empty
  )

// AST of SmithyQL language (plus DSL and minor utilities for building these)
lazy val ast = module("ast")

// Source code model (comments, locations, etc.)
lazy val source = module("source")
  .dependsOn(ast)

// Parser interface / implementation
lazy val parser = module("parser")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "1.1.0",
      "io.circe" %% "circe-generic" % "0.14.14" % Test,
      "io.circe" %% "circe-parser" % "0.14.14" % Test,
      "co.fs2" %% "fs2-io" % "3.12.0" % Test,
    )
  )
  .dependsOn(
    ast % "test->test;compile->compile",
    source % "test->test;compile->compile",
  )

// Formatter for the SmithyQL language constructs
lazy val formatter = module("formatter")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "paiges-cats" % "0.4.4"
    )
  )
  .dependsOn(
    ast,
    source,
    parser % "test->test",
  )

lazy val examples = module("examples")
  .settings(
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %% "smithy4s-core" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-aws-kernel" % smithy4sVersion.value,
    ),
    publish := false,
    // generated code
    scalacOptions += "-Wconf:cat=deprecation:silent",
  )
  .enablePlugins(Smithy4sCodegenPlugin)

// not named "protocol" to leave space for a potential java-only protocol jar
lazy val protocol4s = module("protocol4s")
  .settings(
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %% "smithy4s-core" % smithy4sVersion.value
    )
  )
  .enablePlugins(Smithy4sCodegenPlugin)

// Most of the core functionality of SmithyQL (compilation, analysis, evaluation)
// also: SmithyQL standard library
lazy val core = module("core")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.6.1",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion,
      "com.disneystreaming.smithy4s" %% "smithy4s-dynamic" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-aws-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" % "smithy4s-protocol" % smithy4sVersion.value % Test,
      "com.disneystreaming.alloy" % "alloy-core" % "0.3.21" % Test,
      "software.amazon.smithy" % "smithy-aws-traits" % "1.60.3" % Test,
    ).pipe(jsoniterFix)
  )
  .dependsOn(
    protocol4s,
    examples % "test->compile",
    pluginCore,
    ast,
    source % "test->test;compile->compile",
    parser % "test->compile;test->test",
    formatter,
  )

// LSP-like interfaces like CodeLensProvider, which are later adapted into actual lsp
// but can be used in other, non-LSP contexts
lazy val languageSupport = module("language-support")
  .dependsOn(core % "test->test;compile->compile", parser)

// LSP features that aren't specific to any given lsp library (lsp4j, langoustine)
lazy val lspKernel = module("lsp-kernel")
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.14",
      "org.http4s" %% "http4s-ember-client" % "0.23.30",
      ("io.get-coursier" % "coursier_2.13" % "2.1.24")
        .exclude("org.scala-lang.modules", "scala-collection-compat_2.13"),
      "org.typelevel" %% "cats-tagless-core" % "0.16.3",
      "org.http4s" %% "http4s-ember-server" % "0.23.30" % Test,
    ).pipe(jsoniterFix),
    (Test / test) := {
      (pluginCore / publishLocal).value
      (pluginSample / publishLocal).value

      (Test / test).value
    },
  )
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoPackage := "playground.lsp.buildinfo",
    buildInfoKeys ++= Seq(version, scalaBinaryVersion),
  )
  .dependsOn(languageSupport)

lazy val lsp = module("lsp")
  .settings(
    libraryDependencies ++= Seq(
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.24.0"
    )
  )
  .dependsOn(lspKernel)

lazy val e2e = module("e2e")
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys ++=
      Seq[BuildInfoKey.Entry[_]]( // do you know how to simplify this? let me know please!
        Def
          .task {
            s"""${(lsp / organization).value}::${(lsp / moduleName).value}:${(lsp / version).value}"""
          }
          // todo: replace with a full publishLocal before e2e in particular gets run (but not before tests run normally)
          .dependsOn(
            lspKernel / publishLocal,
            lsp / publishLocal,
            languageSupport / publishLocal,
            core / publishLocal,
            parser / publishLocal,
            pluginCore / publishLocal,
            source / publishLocal,
            ast / publishLocal,
            formatter / publishLocal,
            protocol4s / publishLocal,
          )
          .taskValue
          .named("lspArtifact")
      ),
    publish / skip := true,
    Test / fork := true,
  )
  .dependsOn(lsp)

val writeVersion = taskKey[Unit]("Writes the current version to the `.version` file")

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    mimaFailOnNoPrevious := false,
    mimaPreviousArtifacts := Set.empty,
    addCommandAlias("ci", "+test;+mimaReportBinaryIssues;+publishLocal;writeVersion"),
    writeVersion := {
      IO.write(file(".version"), version.value)
    },
  )
  .aggregate(
    ast,
    source,
    core,
    examples,
    parser,
    formatter,
    languageSupport,
    lspKernel,
    lsp,
    protocol4s,
    pluginCore,
    pluginSample,
    e2e,
  )
