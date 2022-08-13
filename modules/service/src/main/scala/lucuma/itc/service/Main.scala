// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Applicative
import cats.Parallel
import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s._
import lucuma.graphql.routes.GrackleGraphQLService
import lucuma.graphql.routes.Routes
import lucuma.itc.ItcImpl
import lucuma.itc.service.config.ExecutionEnvironment._
import lucuma.itc.service.config._
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import natchez.http4s.NatchezMiddleware
import natchez.http4s.implicits._
import natchez.log.Log
import org.http4s.HttpApp
import org.http4s._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Server
import org.http4s.server.middleware.CORS
import org.http4s.server.middleware.CORSPolicy
import org.http4s.server.middleware.GZip
import org.http4s.server.middleware.{Logger => Http4sLogger}
import org.http4s.server.staticcontent._
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

// #server
object Main extends IOApp {
  val ServiceName = "lucuma-itc"

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](cfg: Config): F[Unit] =
    val banner =
      s"""|
            |   / /_  _________  ______ ___  ____ _      (_) /______
            |  / / / / / ___/ / / / __ `__ \\/ __ `/_____/ / __/ ___/
            | / / /_/ / /__/ /_/ / / / / / / /_/ /_____/ / /_/ /__
            |/_/\\__,_/\\___/\\__,_/_/ /_/ /_/\\__,_/     /_/\\__/\\___/
            |
            | old-itc-url ${cfg.itcUrl}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))

  /** A middleware that adds CORS headers. In production the origin must match the cookie domain. */
  def cors(env: ExecutionEnvironment, domain: Option[String]): CORSPolicy =
    env match
      case Local | Review | Staging =>
        CORS.policy
      case Production               =>
        CORS.policy
          .withAllowOriginHostCi(domain.contains)

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

  def serverResource[F[_]: Async](
    app: WebSocketBuilder2[F] => HttpApp[F],
    cfg: Config
  ): Resource[F, Server] =
    // Spin up the server ...
    EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(Port.fromInt(cfg.port).get)
      .withHttpWebSocketApp(app)
      .build

  def routes[F[_]: Async: Parallel: Trace](
    cfg: Config
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for
      given Logger[F] <- Resource.eval(Slf4jLogger.create[F])
      itc             <- ItcImpl.forUri(cfg.itcUrl)
      mapping         <- Resource.eval(ItcMapping(cfg.environment, itc))
    yield wsb =>
      // Routes for the ITC GraphQL service
      NatchezMiddleware.server(
        GZip(
          cors(cfg.environment, none)(
            Routes.forService(_ => GrackleGraphQLService[F](mapping).some.pure[F], wsb, "itc")
          )
        )
      )

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down in
   * cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Parallel: Logger](cfg: Config): Resource[F, ExitCode] =
    for
      _  <- Resource.eval(banner(cfg))
      ep <- entryPointResource(cfg.honeycomb)
      ap <- ep.wsLiftR(routes(cfg)).map(_.map(_.orNotFound))
      _  <- serverResource(ap, cfg)
    yield ExitCode.Success

  def run(args: List[String]): IO[ExitCode] =
    for
      cfg              <- Config.config.load[IO]
      given Logger[IO] <- Slf4jLogger.create[IO]
      _                <- server[IO](cfg).use(_ => IO.never[ExitCode])
    yield ExitCode.Success
}
