// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.effect._
import cats.implicits._
import edu.gemini.grackle.Mapping
import io.circe._
import natchez.Trace
import org.http4s.CacheDirective
import org.http4s.Headers
import org.http4s.HttpRoutes
import org.http4s.InvalidMessageBodyFailure
import org.http4s.Method
import org.http4s.ParseFailure
import org.http4s.QueryParamDecoder
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Cache-Control`

import scala.concurrent.duration._

trait ItcService[F[_]] {
  def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json]

}

object ItcService {
  def routes[F[_]: Concurrent: Trace](
    service: ItcService[F]
  ): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    implicit val jsonQPDecoder: QueryParamDecoder[Json] = QueryParamDecoder[String].emap { s =>
      parser.parse(s).leftMap { case ParsingFailure(msg, _) =>
        ParseFailure("Invalid variables", msg)
      }
    }

    object QueryMatcher         extends QueryParamDecoderMatcher[String]("query")
    object OperationNameMatcher extends OptionalQueryParamDecoderMatcher[String]("operationName")
    object VariablesMatcher     extends OptionalValidatingQueryParamDecoderMatcher[Json]("variables")

    def runGraphQL(
      op:     Option[String],
      vs:     Option[Json],
      query:  String,
      method: Method
    ): F[Json] =
      Trace[F].span("graphql") {
        for {
          _    <- Trace[F].put("graphql.query" -> query.toString)
          _    <- Trace[F].put("graphql.method" -> method.toString)
          _    <- op.traverse(s => Trace[F].put("graphql.operationName" -> s))
          _    <- vs.traverse(j => Trace[F].put("graphql.variables" -> j.spaces2))
          json <- service.runQuery(op, vs, query)
        } yield json
      }

    HttpRoutes.of[F] {
      // GraphQL query is embedded in the URI query string when queried via GET
      case GET -> Root / "itc" :? QueryMatcher(query) +& OperationNameMatcher(
            op
          ) +& VariablesMatcher(vars0) =>
        vars0.sequence.fold(
          errors => BadRequest(errors.map(_.sanitized).mkString_("", ",", "")),
          vars =>
            for {
              result <- runGraphQL(op, vars, query, GET)
              resp   <- Ok(result, Headers(`Cache-Control`(CacheDirective.`max-age`(1.days))))
            } yield resp
        )

      // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / "itc" =>
        for {
          body   <- req.as[Json]
          obj    <- body.asObject.liftTo[F](InvalidMessageBodyFailure("Invalid GraphQL query"))
          query  <- obj("query")
                      .flatMap(_.asString)
                      .liftTo[F](InvalidMessageBodyFailure("Missing query field"))
          op      = obj("operationName").flatMap(_.asString)
          vars    = obj("variables")
          result <- runGraphQL(op, vars, query, POST)
          resp   <- Ok(result)
        } yield resp
    }
  }

  def service[F[_]: Async](mapping: Mapping[F]): ItcService[F] =
    new ItcService[F] {
      def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json] =
        mapping.compileAndRun(query, op, vars)
    }

}
