package lucuma.itc

import cats.effect._
import cats.syntax.all._
import fs2._
import org.http4s._
import org.http4s.dsl._
import org.http4s.implicits._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware
import org.http4s.server.staticcontent._
import scala.concurrent.ExecutionContext.global
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import lucuma.itc.shared._

object Main extends IOApp {
  def run(args: List[String]) =
    for {
      cfg <- Config.fromCiris.load(Async[IO])
      log <- Slf4jLogger.create[IO]
      _ <- ItcServer.stream[IO](log, Async[IO]).compile.drain
    } yield ExitCode.Success
}

object ItcServer {
  import Codec._

  def routes[F[_]: Logger: Concurrent](service: ItcService): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    //
    //   implicit val jsonQPDecoder: QueryParamDecoder[Json] =
    //     QueryParamDecoder[String].emap { s =>
    //       parser.parse(s).leftMap { case ParsingFailure(msg, _) =>
    //         ParseFailure("Invalid variables", msg)
    //       }
    //     }
    //
    //   object QueryMatcher extends QueryParamDecoderMatcher[String]("query")
    //   object OperationNameMatcher
    //       extends OptionalQueryParamDecoderMatcher[String]("operationName")
    //   object VariablesMatcher
    //       extends OptionalValidatingQueryParamDecoderMatcher[Json]("variables")
    //
    HttpRoutes.of[F] {
      //     // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / "json" =>
        for {
          body <- req.as[ItcParameters].adaptErr {
            case x => x.printStackTrace();x
          }
          resp <- Logger[F].warn(body.toString) *> Logger[F].info(service.calculate(body, true).toString) *> Ok(body.toString)
        } yield resp
      //       for {
      //         body <- req.as[Json]
      //         obj <- body.asObject.liftTo[F](
      //           InvalidMessageBodyFailure("Invalid GraphQL query")
      //         )
      //         query <- obj("query")
      //           .flatMap(_.asString)
      //           .liftTo[F](InvalidMessageBodyFailure("Missing query field"))
      //         op = obj("operationName").flatMap(_.asString)
      //         vars = obj("variables")
      //         result <- service.runQuery(op, vars, query)
      //         resp <- Ok(result)
      //       } yield resp
    }
  }
  //
  def stream[F[_]: Logger: Async]: Stream[F, Nothing] = {
    val itcService = ItcService()

    val httpApp0 = (
      // Routes for static resources, ie. GraphQL Playground
      resourceServiceBuilder[F]("/assets").toRoutes <+>
        routes[F](itcService)
      ).orNotFound

    val httpApp = middleware.Logger.httpApp(true, false)(httpApp0)

    // Spin up the server ...
    for {
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
    } yield exitCode
  }.drain
}
