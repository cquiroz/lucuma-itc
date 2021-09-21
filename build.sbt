// import com.typesafe.sbt.packager.docker.Cmd

val cirisVersion = "2.1.0"
val catsVersion = "2.6.1"
val catsEffectVersion = "3.2.3"
val circeVersion = "0.14.1"
val http4sVersion = "0.23.0-RC1"
val kindProjectorVersion = "0.13.0"
val log4catsVersion = "2.1.1"
val logbackVersion = "1.2.5"
val lucumaCoreVersion = "0.13.1+1-a01bf3c8-SNAPSHOT"
val coulombVersion        = "0.5.6"

ThisBuild / Test / bspEnabled := false
ThisBuild / ScalafixConfig / bspEnabled.withRank(KeyRanks.Invisible) := false

// Plugins
// enablePlugins(JavaAppPackaging, DockerPlugin)

inThisBuild(
  Seq(
    homepage := Some(url("https://github.com/gemini-hlsw/itc-server")),
    Global / onChangedBuildSource := ReloadOnSourceChanges
  ) ++ lucumaPublishSettings
)

// Project Settings
name := "itc-server"
scalaVersion := "3.0.1"

// Docker Settings
// Docker / packageName := "web"
// dockerRepository     := Some("registry.heroku.com")
// dockerUsername       := Some("gemini-new-itc")
// Docker / name        := "web"
// dockerAlias          := DockerAlias(
//   dockerRepository.value,
//   dockerUsername.value,
//   (Docker / name).value,
//   None
// )
// dockerCommands   := {"""
//   FROM openjdk:8
//   WORKDIR /opt/docker
//   ADD --chown=daemon:daemon opt /opt
//   USER daemon
//   CMD ["/opt/docker/bin/itc-server"]
// """.lines.map(_.trim).map((s: String) => Cmd(s)).toSeq}
//

lazy val basic = project
  .in(file("modules/basic"))
  .settings(commonLibSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "is.cir" %% "ciris" % cirisVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
    )
  )
// lazy val service = project
//   .in(file("modules/service"))
//   .dependsOn(legacy)
//   // .settings(commonSettings: _*)
//   .settings(commonLibSettings: _*)
//   // .settings(testkitLibSettings: _*)
//   // .jsSettings(commonModuleTest: _*)
//   // .jvmSettings(commonJVMSettings)
//   .settings(
//     libraryDependencies ++= Seq(
//       "is.cir" %% "ciris" % cirisVersion,
//       "org.http4s" %% "http4s-blaze-server" % http4sVersion,
//       "org.http4s" %% "http4s-blaze-client" % http4sVersion,
//       "org.http4s" %% "http4s-circe" % http4sVersion,
//       "org.http4s" %% "http4s-dsl" % http4sVersion,
//       "org.typelevel" %% "log4cats-slf4j" % log4catsVersion,
//       "ch.qos.logback" % "logback-classic" % logbackVersion,
//     )
//   )
//
lazy val legacy = project
  .in(file("modules/legacy"))
  .settings(commonLibSettings: _*)

// lazy val model = project
//   .in(file("modules/model"))
//   // .dependsOn(model)
//   // .settings(commonSettings: _*)
//   .settings(commonLibSettings: _*)
// .settings(testkitLibSettings: _*)
// .jsSettings(commonModuleTest: _*)
// .jvmSettings(commonJVMSettings)

lazy val commonLibSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-effect" % catsEffectVersion,
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "edu.gemini" %% "lucuma-core" % lucumaCoreVersion,
      "com.manyangled"    %% "coulomb"                    % coulombVersion,
      "com.manyangled"    %% "coulomb-si-units"           % coulombVersion,
      // "com.manyangled"    %%% "coulomb-accepted-units"     % coulombVersion,
      // "com.manyangled"    %%% "coulomb-time-units"         % coulombVersion,
      "com.manyangled"    %% "coulomb-cats"               % coulombVersion,
      "com.manyangled"    %% "coulomb-refined"            % coulombVersion,
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
      // "com.manyangled"    %%% "coulomb-physical-constants" % coulombVersion,
  )
)
