// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

// import lucuma.odb.api.schema.ItcSchema
// import lucuma.odb.api.repo.ItcRepo
// import lucuma.odb.itc.Itc

import cats._
import cats.implicits._
import cats.effect.Concurrent
import io.circe._
// import lucuma.core.model.User
// import sangria.execution._
// import sangria.marshalling.circe._
import org.http4s.{ HttpRoutes, InvalidMessageBodyFailure, ParseFailure, QueryParamDecoder }
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
// import sangria.streaming
// import sangria.streaming.SubscriptionStream

// import scala.concurrent.ExecutionContext.Implicits.global
// import scala.util.control.NonFatal
// import scala.util.{Failure, Success}object StarWarsMapping extends GenericMapping[Id] {
trait ItcService[F[_]] {
  def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json]

  // def query(request: ParsedGraphQLRequest): F[Either[Throwable, Json]]
  //
  // def subscribe(user: Option[User], request: ParsedGraphQLRequest): F[Stream[F, Either[Throwable, Json]]]
  //
}

object ItcService {
  def routes[F[_]: Concurrent](service: ItcService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    implicit val jsonQPDecoder: QueryParamDecoder[Json] = QueryParamDecoder[String].emap { s =>
      parser.parse(s).leftMap { case ParsingFailure(msg, _) => ParseFailure("Invalid variables", msg) }
    }

    object QueryMatcher extends QueryParamDecoderMatcher[String]("query")
    object OperationNameMatcher extends OptionalQueryParamDecoderMatcher[String]("operationName")
    object VariablesMatcher extends OptionalValidatingQueryParamDecoderMatcher[Json]("variables")

    HttpRoutes.of[F] {
      // GraphQL query is embedded in the URI query string when queried via GET
      case GET -> Root / "itc" :?  QueryMatcher(query) +& OperationNameMatcher(op) +& VariablesMatcher(vars0) =>
        vars0.sequence.fold(
          errors => BadRequest(errors.map(_.sanitized).mkString_("", ",", "")),
          vars =>
            for {
              result <- service.runQuery(op, vars, query)
              resp   <- Ok(result)
            } yield resp
          )

      // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / "itc" =>
        for {
          body   <- req.as[Json]
          obj    <- body.asObject.liftTo[F](InvalidMessageBodyFailure("Invalid GraphQL query"))
          query  <- obj("query").flatMap(_.asString).liftTo[F](InvalidMessageBodyFailure("Missing query field"))
          op     =  obj("operationName").flatMap(_.asString)
          vars   =  obj("variables")
          result <- service.runQuery(op, vars, query)
          resp   <- Ok(result)
        } yield resp
    }
  }

  def service[F[_]: Applicative]: ItcService[F] =
    new ItcService[F]{
      def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json] =
        ItcMapping.compileAndRun(query, op, vars).pure[F]
    }

  // def apply[F[_]: Parallel: Async: Itc: Logger](repo: ItcRepo[F]): ItcService[F] =
  //
  //   new ItcService[F] {
  //
  //     override def query(request: ParsedGraphQLRequest): F[Either[Throwable, Json]] =
  //
  //       Dispatcher[F].use { implicit d =>
  //         Async[F].async_ { (cb: Either[Throwable, Json] => Unit) =>
  //           Executor.execute(
  //             schema           = ItcSchema[F],
  //             queryAst         = request.query,
  //             userContext      = repo,
  //             operationName    = request.op,
  //             variables        = request.vars.getOrElse(Json.fromJsonObject(JsonObject())),
  //             exceptionHandler = ItcSchema.exceptionHandler
  //           ).onComplete {
  //             case Success(value) => cb(Right(value))
  //             case Failure(error) => cb(Left(error))
  //           }
  //         }.attempt
  //       }
  //
  //     override def subscribe(
  //       user:    Option[User],
  //       request: ParsedGraphQLRequest
  //     ): F[Stream[F, Either[Throwable, Json]]] =
  //       ???
  //       // Stream.empty[Either[Throwable, Json]].covary[F].pure[F]
  //       // cats.Applicative[F].u
  //
  //       // implicit def subStream(implicit D: Dispatcher[F]): SubscriptionStream[Stream[F, *]] =
  //       //   streaming.fs2.fs2SubscriptionStream[F](D, Async[F])
  //       //
  //       // import sangria.execution.ExecutionScheme.Stream
  //       //
  //       // Dispatcher[F].use { implicit d =>
  //       //   Async[F].fromFuture {
  //       //     Async[F].delay {
  //       //       Executor.prepare(
  //       //         schema = ItcSchema[F](),
  //       //         queryAst = request.query,
  //       //         // userContext = odb,
  //       //         operationName = request.op,
  //       //         variables = request.vars.getOrElse(Json.fromJsonObject(JsonObject())),
  //       //         exceptionHandler = ItcSchema.exceptionHandler
  //       //       ).map { preparedQuery =>
  //       //         preparedQuery
  //       //           .execute()
  //       //           .evalTap(n => info(user, s"Subscription event: ${n.printWith(Printer.spaces2)}"))
  //       //           .map(_.asRight[Throwable])
  //       //           .recover { case NonFatal(error) => error.asLeft[Json] }
  //       //       }
  //       //     }
  //       //   }
  //       // }
  //
  //     // }
  //
  //   }
}
