// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.core.model.User
import lucuma.sso.client.SsoClient
import lucuma.itc.ItcImpl

import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.Parallel
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._
import org.typelevel.log4cats.{Logger => Log4CatsLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import edu.gemini.grackle.Mapping

import scala.concurrent.ExecutionContext.global
import lucuma.odb.itc.Itc
import lucuma.odb.api.repo.ItcRepo

// #server
object Main extends IOApp {

  def stream[F[_]: Log4CatsLogger: Parallel: Async: Itc](
    mapping: Mapping[F],
    cfg: Config
  ): Stream[F, Nothing] = {
    val itcService   = ItcService.service[F](mapping)

    def app(userClient: SsoClient[F, User]): HttpApp[F] =
      Logger.httpApp(logHeaders = true, logBody = false)((

        // Routes for static resources, ie. GraphQL Playground
        resourceServiceBuilder[F]("/assets").toRoutes <+>

          // Routes for the ITC GraphQL service
          ItcService.routes[F](itcService)

      ).orNotFound)

    // Spin up the server ...
    for {
      sso       <- Stream.resource(cfg.ssoClient[F])
      userClient = sso.map(_.user)
      exitCode  <- BlazeServerBuilder[F](global)
        .bindHttp(cfg.port, "0.0.0.0")
        .withHttpApp(app(userClient))
        .withWebSockets(true)
        .serve
    } yield exitCode
  }.drain

  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg  <- Config.fromCiris.load(Async[IO])
      log  <- Slf4jLogger.create[IO]
      map  <- ItcMapping[IO]
      _    <- ItcImpl.forHeroku[IO].use { itc =>
        stream(map, cfg)(log, Parallel[IO], Async[IO], itc).compile.drain
      }
    } yield ExitCode.Success
}

