// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import lucuma.itc.ItcImpl

import cats.effect.{ Async, ExitCode, IO, IOApp }
import cats.implicits._
import fs2.Stream
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._
// import org.typelevel.log4cats.{Logger => Log4CatsLogger}
// import org.typelevel.log4cats.slf4j.Slf4jLogger
import edu.gemini.grackle.Mapping

import scala.concurrent.ExecutionContext.global
import lucuma.itc.Itc

// #server
object Main extends IOApp {

  def stream[F[_]: Async](
    mapping: Mapping[F],
    itc:     Itc[F],
    cfg:     Config
  ): Stream[F, Nothing] = {
    val itcService = ItcService.service[F](mapping, itc)

    def app: HttpApp[F] =
      Logger.httpApp(logHeaders = true, logBody = false)(
        (
          // Routes for static resources, ie. GraphQL Playground
          resourceServiceBuilder[F]("/assets").toRoutes <+>

            // Routes for the ITC GraphQL service
            ItcService.routes[F](itcService)
        ).orNotFound
      )

    // Spin up the server ...
    for {
      exitCode <- BlazeServerBuilder[F](global)
                    .bindHttp(cfg.port, "0.0.0.0")
                    .withHttpApp(app)
                    .serve
    } yield exitCode
  }.drain

  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg <- Config.fromCiris.load(Async[IO])
      // log  <- Slf4jLogger.create[IO]
      map <- ItcMapping[IO]
      _   <- ItcImpl.forHeroku[IO].use { itc =>
               stream(map, itc, cfg).compile.drain
             }
    } yield ExitCode.Success
}
