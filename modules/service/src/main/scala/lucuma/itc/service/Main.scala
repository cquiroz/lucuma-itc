// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Applicative
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
import org.http4s.server.middleware.{ Logger => Http4sLogger }
import org.http4s.server.staticcontent._

import scala.concurrent.ExecutionContext.global
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger

// #server
object Main extends IOApp {

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger]: F[Unit] = {
    val banner =
      s"""|
            |   / /_  _________  ______ ___  ____ _      (_) /______
            |  / / / / / ___/ / / / __ `__ \\/ __ `/_____/ / __/ ___/
            | / / /_/ / /__/ /_/ / / / / / / /_/ /_____/ / /_/ /__
            |/_/\\__,_/\\___/\\__,_/_/ /_/ /_/\\__,_/     /_/\\__/\\___/
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))
  }

  def stream[F[_]: Async](
    mapping: Mapping[F],
    cfg:     Config
  ): Stream[F, Nothing] = {
    val itcService = ItcService.service[F](mapping)

    def app: HttpApp[F] =
      Http4sLogger.httpApp(logHeaders = true, logBody = false)(
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
      log <- Slf4jLogger.create[IO]
      _   <- { implicit val l = log; banner[IO] }
      _   <- ItcImpl.forHeroku[IO].use {
               ItcMapping[IO](_).flatMap { map =>
                 stream(map, cfg).compile.drain
               }
             }
    } yield ExitCode.Success
}
