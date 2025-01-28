import NativePackagerHelper._

val boopickleVersion            = "1.5.0"
val catsVersion                 = "2.13.0"
val catsEffectVersion           = "3.5.7"
val catsTestkitScalaTestVersion = "2.1.5"
val catsTimeVersion             = "0.3.4"
val catsScalacheckVersion       = "0.3.2"
val circeVersion                = "0.14.10"
val circeRefinedVersion         = "0.15.1"
val cirisVersion                = "3.7.0"
val clueVersion                 = "0.43.0"
val disciplineMunitVersion      = "2.0.0"
val fs2Version                  = "3.11.0"
val gatlingVersion              = "3.13.1"
val grackleVersion              = "0.23.0"
val graphQLRoutesVersion        = "0.8.17"
val http4sVersion               = "0.23.30"
val http4sJdkHttpClientVersion  = "0.10.0"
val kindProjectorVersion        = "0.13.2"
val kittensVersion              = "3.4.0"
val log4catsVersion             = "2.7.0"
val lucumaCoreVersion           = "0.114.1"
val lucumaRefinedVersion        = "0.1.3"
val monocleVersion              = "3.3.0"
val munitVersion                = "1.1.0"
val munitCatsEffectVersion      = "2.0.0"
val natchezVersion              = "0.3.7"
val natchezHttp4sVersion        = "0.6.1"
val pprintVersion               = "0.9.0"
val redis4CatsVersion           = "1.7.2"
val refinedVersion              = "0.11.3"
val slf4jVersion                = "2.0.16"
val spireVersion                = "0.18.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion        := "3.6.3"
ThisBuild / crossScalaVersions  := Seq("3.6.3")
ThisBuild / tlBaseVersion       := "0.26"
ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / scalacOptions ++= Seq("-Xmax-inlines", "50") // Hash derivation fails with default of 32

Global / onChangedBuildSource := ReloadOnSourceChanges

enablePlugins(NoPublishPlugin)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports; Test/scalafix OrganizeImports; scalafmtAll"
)

lazy val commonSettings = lucumaGlobalSettings ++ Seq(
  Test / parallelExecution := false // tests run fine in parallel but output is nicer this way
)

ThisBuild / watchOnTermination := { (action, cmd, times, state) =>
  val projNames = cmd
    .split(";")
    .flatMap(
      Some(_)
        .filter(_.contains("/reStart"))
        .flatMap(_.trim.split("/reStart") match {
          case Array(projName) => Some(projName)
          case _               => None
        })
    )
  projNames.foldLeft(state) { (acc, projName) =>
    val projRef = ProjectRef((ThisBuild / baseDirectory).value, projName)
    Project.extract(state).runTask(projRef / reStop, state)._1
  }
}

ThisBuild / githubWorkflowBuild +=
  WorkflowStep.Use(
    UseRef.Public("kamilkisiela", "graphql-inspector", "master"),
    name = Some("Validate GraphQL schema changes"),
    params = Map("schema"        -> "main:modules/service/src/main/resources/graphql/itc.graphql",
                 "approve-label" -> "expected-breaking-change"
    ),
    cond = Some("github.event_name == 'pull_request'")
  )

// Basic model classes
lazy val model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/model"))
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc",
    libraryDependencies ++= Seq(
      "io.circe"      %%% "circe-generic" % circeVersion,
      "io.circe"      %%% "circe-refined" % circeRefinedVersion,
      "org.typelevel" %%% "cats-core"     % catsVersion,
      "edu.gemini"    %%% "lucuma-core"   % lucumaCoreVersion,
      "eu.timepit"    %%% "refined"       % refinedVersion,
      "eu.timepit"    %%% "refined-cats"  % refinedVersion,
      "org.typelevel" %%% "kittens"       % kittensVersion
    )
  )

lazy val ocslibHash = taskKey[String]("hash of ocslib")
ThisBuild / ocslibHash / fileInputs += (service / baseDirectory).value.toGlob / "ocslib" / "*.jar"
ThisBuild / ocslibHash := {
  val hashes = ocslibHash.inputFiles.sorted.map(_.toFile).map(Hash(_))
  Hash.toHex(Hash(hashes.toArray.flatten))
}

// Contains the grackle server
lazy val service = project
  .in(file("modules/service"))
  .dependsOn(model.jvm)
  .settings(commonSettings)
  .settings(
    name                  := "lucuma-itc-service",
    scalacOptions -= "-Vtype-diffs",
    reStart / javaOptions := Seq("-Dcats.effect.stackTracing=DISABLED",
                                 "-Dcats.effect.tracing.mode=none"
    ),
    reStart / envVars     := Map(
      "ITC_URL"        -> "https://itc-server-exp.herokuapp.com/",
      "REDISCLOUD_URL" -> "redis://localhost"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "grackle-core"          % grackleVersion,
      "org.typelevel"  %% "grackle-generic"       % grackleVersion,
      "org.typelevel"  %% "grackle-circe"         % grackleVersion,
      "edu.gemini"     %% "lucuma-graphql-routes" % graphQLRoutesVersion,
      "org.tpolecat"   %% "natchez-honeycomb"     % natchezVersion,
      "org.tpolecat"   %% "natchez-log"           % natchezVersion,
      "org.tpolecat"   %% "natchez-http4s"        % natchezHttp4sVersion,
      "co.fs2"         %% "fs2-core"              % fs2Version,
      "edu.gemini"     %% "lucuma-core"           % lucumaCoreVersion,
      "org.typelevel"  %% "cats-core"             % catsVersion,
      "org.typelevel"  %% "cats-effect"           % catsEffectVersion,
      "is.cir"         %% "ciris"                 % cirisVersion,
      "org.typelevel"  %% "log4cats-slf4j"        % log4catsVersion,
      "org.slf4j"       % "slf4j-simple"          % slf4jVersion,
      "org.http4s"     %% "http4s-core"           % http4sVersion,
      "org.http4s"     %% "http4s-ember-server"   % http4sVersion,
      "eu.timepit"     %% "refined"               % refinedVersion,
      "eu.timepit"     %% "refined-cats"          % refinedVersion,
      "dev.profunktor" %% "redis4cats-effects"    % redis4CatsVersion,
      "dev.profunktor" %% "redis4cats-log4cats"   % redis4CatsVersion,
      "com.lihaoyi"    %% "pprint"                % pprintVersion,
      "io.suzaku"      %% "boopickle"             % boopickleVersion,
      "org.typelevel"  %% "munit-cats-effect"     % munitCatsEffectVersion % Test
    ),
    buildInfoKeys         := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "herokuSourceVersion" -> sys.env.get("SOURCE_VERSION"),
      "buildDateTime"       -> System.currentTimeMillis(),
      ocslibHash
    ),
    Universal / mappings ++= {
      val dir = baseDirectory.value / "ocslib"
      (dir ** AllPassFilter).pair(relativeTo(dir.getParentFile))
    }
  )
  .enablePlugins(JavaAppPackaging, BuildInfoPlugin)

lazy val client = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/client"))
  .dependsOn(model)
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc-client",
    libraryDependencies ++= Seq(
      "edu.gemini"    %%% "lucuma-core"       % lucumaCoreVersion,
      "edu.gemini"    %%% "lucuma-refined"    % lucumaRefinedVersion,
      "org.typelevel" %%% "cats-core"         % catsVersion,
      "org.typelevel" %%% "cats-effect"       % catsEffectVersion,
      "org.http4s"    %%% "http4s-circe"      % http4sVersion,
      "org.http4s"    %%% "http4s-dsl"        % http4sVersion,
      "io.circe"      %%% "circe-literal"     % circeVersion,
      "edu.gemini"    %%% "clue-model"        % clueVersion,
      "edu.gemini"    %%% "clue-http4s"       % clueVersion,
      "edu.gemini"    %%% "clue-core"         % clueVersion,
      "io.circe"      %%% "circe-generic"     % circeVersion,
      "org.tpolecat"  %%% "natchez-http4s"    % natchezHttp4sVersion,
      "org.typelevel" %%% "spire"             % spireVersion,
      "org.typelevel" %%% "spire-extras"      % spireVersion,
      "org.typelevel" %%% "kittens"           % kittensVersion,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % Test,
      "com.lihaoyi"   %%% "pprint"            % pprintVersion          % Test
    )
  )

lazy val benchmark = project
  .in(file("modules/benchmarks"))
  .enablePlugins(GatlingPlugin, NoPublishPlugin)
  .settings(
    libraryDependencies ++= Seq(
      // ("io.suzaku" %% "boopickle" % "1.4.0").cross(CrossVersion.for3Use2_13),
      ("io.gatling" % "gatling-test-framework" % gatlingVersion)
        .exclude("org.typelevel", "spire-macros_2.13")
        .exclude("io.suzaku", "boopickle_2.13")
        .exclude("org.scala-lang.modules", "scala-collection-compat_2.13")
    )
  )
  .dependsOn(service)

val MUnitFramework = new TestFramework("munit.Framework")

lazy val testkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/testkit"))
  .dependsOn(client)
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc-testkit",
    libraryDependencies ++= Seq(
      "edu.gemini"        %%% "lucuma-core-testkit" % lucumaCoreVersion,
      "org.typelevel"     %%% "cats-testkit"        % catsVersion,
      "dev.optics"        %%% "monocle-law"         % monocleVersion,
      "org.typelevel"     %%% "spire-laws"          % spireVersion,
      "eu.timepit"        %%% "refined-scalacheck"  % refinedVersion,
      "io.circe"          %%% "circe-testing"       % circeVersion,
      "io.chrisdavenport" %%% "cats-scalacheck"     % catsScalacheckVersion
    )
  )

lazy val tests = project
  .in(file("modules/tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(service, client.jvm, testkit.jvm)
  .settings(
    name := "lucuma-itc-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "munit-cats-effect"      % munitCatsEffectVersion     % Test,
      "com.lihaoyi"   %%% "pprint"                 % pprintVersion              % Test,
      "org.http4s"     %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion % Test,
      "org.typelevel" %%% "log4cats-slf4j"         % log4catsVersion            % Test,
      "org.scalameta" %%% "munit"                  % munitVersion               % Test,
      "org.typelevel" %%% "discipline-munit"       % disciplineMunitVersion     % Test
    ),
    testFrameworks += MUnitFramework
  )
