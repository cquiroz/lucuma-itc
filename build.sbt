import NativePackagerHelper._

val catsEffectVersion           = "3.4.8"
val catsTestkitScalaTestVersion = "2.1.5"
val catsVersion                 = "2.9.0"
val catsScalacheckVersion       = "0.3.2"
val catsTimeVersion             = "0.3.4"
val circeVersion                = "0.14.5"
val cirisVersion                = "3.1.0"
val clueVersion                 = "0.25.1"
val http4sVersion               = "0.23.18"
val http4sJdkHttpClientVersion  = "0.9.0"
val fs2Version                  = "3.6.1"
val kindProjectorVersion        = "0.13.2"
val lucumaCoreVersion           = "0.70.0"
val lucumaRefinedVersion        = "0.1.1"
val slf4jVersion                = "2.0.6"
val log4catsVersion             = "2.5.0"
val monocleVersion              = "3.2.0"
val munitCatsEffectVersion      = "1.0.7"
val graphQLRoutesVersion        = "0.5.11"
val refinedVersion              = "0.10.2"
val grackleVersion              = "0.10.3"
val natcchezHttp4sVersion       = "0.5.0"
val natchezVersion              = "0.3.1"
val munitVersion                = "0.7.29"
val disciplineMunitVersion      = "1.0.9"
val gatlingVersion              = "3.9.2"
val spireVersion                = "0.18.0"
val redis4CatsVersion           = "1.4.0"
val pprintVersion               = "0.8.1"
val kittensVersion              = "3.0.0"
val boopickleVersion            = "1.4.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion        := "3.2.2"
ThisBuild / crossScalaVersions  := Seq("3.2.2")
ThisBuild / tlBaseVersion       := "0.7"
ThisBuild / tlCiReleaseBranches := Seq("master")

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

// Basic model classes
lazy val model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/model"))
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion
    )
  )

// Contains ITC logic and conectivity to the old itc server
lazy val core = project
  .in(file("modules/itc"))
  .dependsOn(model.jvm)
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc-core",
    libraryDependencies ++= Seq(
      "edu.gemini"     %% "lucuma-core"         % lucumaCoreVersion,
      "edu.gemini"    %%% "lucuma-refined"      % lucumaRefinedVersion,
      "org.typelevel"  %% "cats-core"           % catsVersion,
      "org.typelevel"  %% "cats-effect"         % catsEffectVersion,
      "org.http4s"     %% "http4s-circe"        % http4sVersion,
      "org.http4s"     %% "http4s-dsl"          % http4sVersion,
      "io.circe"       %% "circe-literal"       % circeVersion,
      "edu.gemini"     %% "clue-model"          % clueVersion,
      "io.circe"       %% "circe-generic"       % circeVersion,
      "org.tpolecat"   %% "natchez-http4s"      % natcchezHttp4sVersion,
      "org.typelevel"  %% "log4cats-slf4j"      % log4catsVersion,
      "org.typelevel" %%% "spire"               % spireVersion,
      "org.typelevel" %%% "spire-extras"        % spireVersion,
      "org.typelevel" %%% "kittens"             % kittensVersion,
      "org.typelevel"  %% "munit-cats-effect-3" % munitCatsEffectVersion % Test,
      "com.lihaoyi"   %%% "pprint"              % pprintVersion          % Test
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
  .dependsOn(core, model.jvm)
  .settings(commonSettings)
  .settings(
    name              := "lucuma-itc-service",
    scalacOptions -= "-Vtype-diffs",
    reStart / envVars := Map(
      "ITC_URL"        -> "https://itc-server-exp.herokuapp.com/",
      "REDISCLOUD_URL" -> "redis://localhost"
    ),
    libraryDependencies ++= Seq(
      "edu.gemini"     %% "gsp-graphql-core"              % grackleVersion,
      "edu.gemini"     %% "gsp-graphql-generic"           % grackleVersion,
      "edu.gemini"     %% "gsp-graphql-circe"             % grackleVersion,
      "edu.gemini"     %% "lucuma-graphql-routes-grackle" % graphQLRoutesVersion,
      "org.tpolecat"   %% "natchez-honeycomb"             % natchezVersion,
      "org.tpolecat"   %% "natchez-log"                   % natchezVersion,
      "org.tpolecat"   %% "natchez-http4s"                % natcchezHttp4sVersion,
      "co.fs2"         %% "fs2-core"                      % fs2Version,
      "edu.gemini"     %% "lucuma-core"                   % lucumaCoreVersion,
      "org.typelevel"  %% "cats-core"                     % catsVersion,
      "org.typelevel"  %% "cats-effect"                   % catsEffectVersion,
      "is.cir"         %% "ciris"                         % cirisVersion,
      "org.typelevel"  %% "log4cats-slf4j"                % log4catsVersion,
      "org.slf4j"       % "slf4j-simple"                  % slf4jVersion,
      "org.http4s"     %% "http4s-core"                   % http4sVersion,
      "org.http4s"     %% "http4s-ember-server"           % http4sVersion,
      "eu.timepit"     %% "refined"                       % refinedVersion,
      "eu.timepit"     %% "refined-cats"                  % refinedVersion,
      "dev.profunktor" %% "redis4cats-effects"            % redis4CatsVersion,
      "dev.profunktor" %% "redis4cats-log4cats"           % redis4CatsVersion,
      "io.suzaku"      %% "boopickle"                     % boopickleVersion,
      "org.typelevel"  %% "munit-cats-effect-3"           % munitCatsEffectVersion % Test
    ),
    buildInfoKeys     := Seq[BuildInfoKey](
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
      "edu.gemini"    %%% "lucuma-core"         % lucumaCoreVersion,
      "edu.gemini"    %%% "lucuma-refined"      % lucumaRefinedVersion,
      "org.typelevel" %%% "cats-core"           % catsVersion,
      "org.typelevel" %%% "cats-effect"         % catsEffectVersion,
      "org.http4s"    %%% "http4s-circe"        % http4sVersion,
      "org.http4s"    %%% "http4s-dsl"          % http4sVersion,
      "io.circe"      %%% "circe-literal"       % circeVersion,
      "edu.gemini"    %%% "clue-model"          % clueVersion,
      "edu.gemini"    %%% "clue-http4s"         % clueVersion,
      "edu.gemini"    %%% "clue-core"           % clueVersion,
      "io.circe"      %%% "circe-generic"       % circeVersion,
      "org.tpolecat"  %%% "natchez-http4s"      % natcchezHttp4sVersion,
      "org.typelevel" %%% "spire"               % spireVersion,
      "org.typelevel" %%% "spire-extras"        % spireVersion,
      "org.typelevel" %%% "kittens"             % kittensVersion,
      "org.typelevel" %%% "munit-cats-effect-3" % munitCatsEffectVersion % Test,
      "com.lihaoyi"   %%% "pprint"              % pprintVersion          % Test
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
      "org.typelevel" %%% "munit-cats-effect-3"    % munitCatsEffectVersion     % Test,
      "com.lihaoyi"   %%% "pprint"                 % pprintVersion              % Test,
      "org.http4s"     %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion % Test,
      "org.typelevel" %%% "log4cats-slf4j"         % log4catsVersion            % Test,
      "org.scalameta" %%% "munit"                  % munitVersion               % Test,
      "org.typelevel" %%% "discipline-munit"       % disciplineMunitVersion     % Test
    ),
    testFrameworks += MUnitFramework
  )
