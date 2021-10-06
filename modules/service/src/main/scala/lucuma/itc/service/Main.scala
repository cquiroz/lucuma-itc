// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Applicative
import cats.effect._
import cats.syntax.all._
import edu.gemini.grackle.Mapping
import fs2.Stream
import lucuma.itc.ItcImpl
import lucuma.itc.service.config._
import natchez.log.Log
import natchez.honeycomb.Honeycomb
import natchez.http4s.implicits._
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.{ Logger => Http4sLogger }
import org.http4s.server.staticcontent._
import org.http4s._
import org.http4s.implicits._

import scala.concurrent.ExecutionContext.global
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import natchez.EntryPoint
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.server.Server

// #server
object Main extends IOApp {
  val ServiceName = "lucuma-itc"

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

  /**
   * A resource that yields a Natchez tracing entry point, either a Honeycomb endpoint if `config`
   * is defined, otherwise a log endpoint.
   */
  def entryPointResource[F[_]: Sync: Logger](
    config: Option[HoneycombConfig]
  ): Resource[F, EntryPoint[F]] =
    config.fold(Log.entryPoint(ServiceName).pure[Resource[F, *]]) { cfg =>
      Honeycomb.entryPoint(ServiceName) { cb =>
        Sync[F].delay {
          cb.setWriteKey(cfg.writeKey)
          cb.setDataset(cfg.dataset)
          cb.build()
        }
      }
    }

  def serverResource[F[_]: Async](app: HttpApp[F], cfg: Config): Resource[F, Server] =
    // Spin up the server ...
    BlazeServerBuilder[F](global)
      .bindHttp(cfg.port, "0.0.0.0")
      .withHttpApp(app)
      .resource

  def routes[F[_]: Async: natchez.Trace](cfg: Config): Resource[F, HttpRoutes[F]] =
    for {
      itc <- ItcImpl.forUri(cfg.itcUrl)
      map <- Resource.eval(ItcMapping(itc))
      its <- Resource.pure(ItcService.service(map))
    } yield

    // Routes for static resources, ie. GraphQL Playground
    resourceServiceBuilder[F]("/assets").toRoutes <+>
      // Routes for the ITC GraphQL service
      ItcService.routes(its)


  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down in
   * cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Logger](cfg: Config): Resource[F, ExitCode] =
    for {
      _  <- Resource.eval(banner)
      ep <- entryPointResource(cfg.honeycomb)
      ap <- ep.liftR(routes(cfg))
      s  <- serverResource(ap, cfg)
    } yield s

  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg <- Config.config.load[IO]
      log <- Slf4jLogger.create[IO]
      _   <- {
        implicit val l = log
        server[IO](cfg)
      }.use(IO.pure)
    } yield ExitCode.Success
}
