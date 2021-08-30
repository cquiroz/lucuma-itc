package lucuma.itc

import cats.effect._
import cats.syntax.all._
import fs2._
import org.http4s.implicits._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._
import scala.concurrent.ExecutionContext.global
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger => CatsLogger}

object Main extends IOApp {
  def run(args: List[String]) =
    for {
      cfg <- Config.fromCiris.load(Async[IO])
      log <- Slf4jLogger.create[IO]
      _ <- ItcServer.stream[IO](log, Async[IO]).compile.drain
    } yield ExitCode.Success
}

object ItcServer {
  def stream[F[_]: CatsLogger: Async]: Stream[F, Nothing] = {
    val itcService = ItcService.service[F]

    val httpApp0 = (
      // Routes for static resources, ie. GraphQL Playground
      resourceServiceBuilder[F]("/assets").toRoutes <+>
        ItcService.routes[F](itcService)
      ).orNotFound

    val httpApp = Logger.httpApp(true, false)(httpApp0)

    // Spin up the server ...
    for {
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
    } yield exitCode
  }.drain
}
