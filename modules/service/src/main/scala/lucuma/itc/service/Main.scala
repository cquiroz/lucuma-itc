// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import edu.gemini.grackle.Mapping
import fs2.Stream
import lucuma.itc.ItcImpl
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._

import scala.concurrent.ExecutionContext.global

// #server
object Main extends IOApp {

  def stream[F[_]: Async](
    mapping: Mapping[F],
    cfg:     Config
  ): Stream[F, Nothing] = {
    val itcService = ItcService.service[F](mapping)

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
      _   <- ItcImpl.forHeroku[IO].use {
               ItcMapping[IO](_).flatMap { map =>
                 stream(map, cfg).compile.drain
               }
             }
    } yield ExitCode.Success
}
