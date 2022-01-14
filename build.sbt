val coulombVersion              = "0.5.7"
val catsEffectVersion           = "3.3.4"
val catsTestkitScalaTestVersion = "2.1.5"
val catsVersion                 = "2.7.0"
val catsScalacheckVersion       = "0.3.1"
val catsTimeVersion             = "0.3.4"
val circeVersion                = "0.14.1"
val cirisVersion                = "2.3.2"
val clueVersion                 = "0.19.1"
val http4sVersion               = "0.23.7"
val http4sJdkHttpClientVersion  = "0.5.0"
val fs2Version                  = "3.2.4"
val kindProjectorVersion        = "0.13.2"
val lucumaCoreVersion           = "0.15.1"
val slf4jVersion                = "1.7.33"
val log4catsVersion             = "2.1.1"
val monocleVersion              = "3.1.0"
val munitCatsEffectVersion      = "1.0.7"
val refinedVersion              = "0.9.28"
val grackleVersion              = "0.1.14"
val natcchezHttp4sVersion       = "0.3.2"
val natchezVersion              = "0.1.6"
val munitVersion                = "0.7.29"
val disciplineMunitVersion      = "1.0.9"
val gatlingVersion              = "3.7.3"

inThisBuild(
  Seq(
    homepage                      := Some(url("https://github.com/gemini-hlsw/lucuma-itc")),
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    addCompilerPlugin(
      ("org.typelevel"             % "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    )
  ) ++ lucumaPublishSettings
)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports; Test/scalafix OrganizeImports; scalafmtAll"
)

lazy val commonSettings = lucumaGlobalSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-testkit"           % catsVersion                 % "test",
    "org.typelevel" %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test"
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  Test / parallelExecution := false, // tests run fine in parallel but output is nicer this way
  scalacOptions --= Seq("-Xfatal-warnings").filterNot(_ => insideCI.value),
  scalacOptions ++= Seq(
    "-Ymacro-annotations",
    "-Ywarn-macros:after"
  )
)

lazy val itc = project
  .in(file("modules/itc"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc",
    libraryDependencies ++= Seq(
      "edu.gemini"     %% "lucuma-core"              % lucumaCoreVersion,
      "org.typelevel"  %% "cats-core"                % catsVersion,
      "org.typelevel"  %% "cats-effect"              % catsEffectVersion,
      "org.http4s"     %% "http4s-async-http-client" % http4sVersion,
      "org.http4s"     %% "http4s-circe"             % http4sVersion,
      "org.http4s"     %% "http4s-dsl"               % http4sVersion,
      "io.circe"       %% "circe-literal"            % circeVersion,
      "edu.gemini"     %% "clue-model"               % clueVersion,
      "io.circe"       %% "circe-generic"            % circeVersion,
      "org.tpolecat"   %% "natchez-http4s"           % natcchezHttp4sVersion,
      "org.typelevel"  %% "log4cats-slf4j"           % log4catsVersion,
      "com.manyangled" %% "coulomb"                  % coulombVersion,
      "com.manyangled" %% "coulomb-cats"             % coulombVersion,
      "org.typelevel"  %% "munit-cats-effect-3"      % munitCatsEffectVersion % Test
    )
  )

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(itc)
  .settings(commonSettings)
  .settings(
    name              := "lucuma-itc-service",
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    scalacOptions -= "-Vtype-diffs",
    reStart / envVars := Map("ITC_URL" -> "https://gemini-new-itc.herokuapp.com/json"),
    libraryDependencies ++= Seq(
      "edu.gemini"    %% "gsp-graphql-core"    % grackleVersion,
      "edu.gemini"    %% "gsp-graphql-generic" % grackleVersion,
      "edu.gemini"    %% "gsp-graphql-circe"   % grackleVersion,
      "org.tpolecat"  %% "natchez-honeycomb"   % natchezVersion,
      "org.tpolecat"  %% "natchez-log"         % natchezVersion,
      "org.tpolecat"  %% "natchez-http4s"      % natcchezHttp4sVersion,
      "co.fs2"        %% "fs2-core"            % fs2Version,
      "edu.gemini"    %% "lucuma-core"         % lucumaCoreVersion,
      "org.typelevel" %% "cats-core"           % catsVersion,
      "org.typelevel" %% "cats-effect"         % catsEffectVersion,
      "is.cir"        %% "ciris"               % cirisVersion,
      "org.typelevel" %% "log4cats-slf4j"      % log4catsVersion,
      "org.slf4j"      % "slf4j-simple"        % slf4jVersion,
      "org.http4s"    %% "http4s-core"         % http4sVersion,
      "org.http4s"    %% "http4s-blaze-server" % http4sVersion,
      "eu.timepit"    %% "refined"             % refinedVersion,
      "eu.timepit"    %% "refined-cats"        % refinedVersion,
      "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion % Test
    )
  )
  .enablePlugins(JavaAppPackaging)

lazy val benchmark = project
  .in(file("modules/benchmarks"))
  .enablePlugins(GatlingPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe"             %% "circe-core"                % circeVersion,
      "io.gatling.highcharts" % "gatling-charts-highcharts" % gatlingVersion % Test,
      "io.gatling"            % "gatling-test-framework"    % gatlingVersion % Test
    )
  )
  .dependsOn(service)
