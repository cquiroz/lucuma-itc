// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.{Applicative}
import cats.effect.Concurrent
import cats.implicits._
import io.circe.{Json, ParsingFailure}
import io.circe.Decoder
import org.http4s._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.syntax.all._
import org.http4s.dsl._
import org.http4s.implicits._
import lucuma.itc.model.ItcParameters

// #service
trait ItcService[F[_]] {
  def runQuery(op: Option[String], vars: Option[Json], query: String): F[Unit]
}

trait ItcParametersCodec {
  implicit val paramsDecoder: Decoder[ItcParameters] = ???
}

object ItcService extends ItcParametersCodec {
  def routes[F[_]: Concurrent](service: ItcService[F]): HttpRoutes[F] = {
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
          body <- req.as[ItcParameters]
          resp <- Ok("ok")
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
  def service[F[_]](implicit F: Applicative[F]): ItcService[F] =
    new ItcService[F] {
      def runQuery(
          op: Option[String],
          vars: Option[Json],
          query: String
      ): F[Unit] =
        Applicative[F].unit
      // StarWarsMapping.compileAndRun(query, op, vars).pure[F]
    }
}
// #service
