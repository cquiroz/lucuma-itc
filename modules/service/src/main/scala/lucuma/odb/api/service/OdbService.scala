// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.odb.api.schema.ItcSchema
import lucuma.odb.api.repo.ItcRepo
import lucuma.odb.itc.Itc

import cats.Parallel
import cats.implicits._
import cats.effect.Async
import cats.effect.std.Dispatcher
import fs2.Stream
import org.typelevel.log4cats.Logger
import io.circe._
import lucuma.core.model.User
import sangria.execution._
import sangria.marshalling.circe._
// import sangria.streaming
// import sangria.streaming.SubscriptionStream

import scala.concurrent.ExecutionContext.Implicits.global
// import scala.util.control.NonFatal
import scala.util.{Failure, Success}


trait ItcService[F[_]] {

  def query(request: ParsedGraphQLRequest): F[Either[Throwable, Json]]

  def subscribe(user: Option[User], request: ParsedGraphQLRequest): F[Stream[F, Either[Throwable, Json]]]

}

object ItcService {

  def apply[F[_]: Parallel: Async: Itc: Logger](repo: ItcRepo[F]): ItcService[F] =

    new ItcService[F] {

      override def query(request: ParsedGraphQLRequest): F[Either[Throwable, Json]] =

        Dispatcher[F].use { implicit d =>
          Async[F].async_ { (cb: Either[Throwable, Json] => Unit) =>
            Executor.execute(
              schema           = ItcSchema[F],
              queryAst         = request.query,
              userContext      = repo,
              operationName    = request.op,
              variables        = request.vars.getOrElse(Json.fromJsonObject(JsonObject())),
              exceptionHandler = ItcSchema.exceptionHandler
            ).onComplete {
              case Success(value) => cb(Right(value))
              case Failure(error) => cb(Left(error))
            }
          }.attempt
        }

      override def subscribe(
        user:    Option[User],
        request: ParsedGraphQLRequest
      ): F[Stream[F, Either[Throwable, Json]]] =
        ???
        // Stream.empty[Either[Throwable, Json]].covary[F].pure[F]
        // cats.Applicative[F].u

        // implicit def subStream(implicit D: Dispatcher[F]): SubscriptionStream[Stream[F, *]] =
        //   streaming.fs2.fs2SubscriptionStream[F](D, Async[F])
        //
        // import sangria.execution.ExecutionScheme.Stream
        //
        // Dispatcher[F].use { implicit d =>
        //   Async[F].fromFuture {
        //     Async[F].delay {
        //       Executor.prepare(
        //         schema = ItcSchema[F](),
        //         queryAst = request.query,
        //         // userContext = odb,
        //         operationName = request.op,
        //         variables = request.vars.getOrElse(Json.fromJsonObject(JsonObject())),
        //         exceptionHandler = ItcSchema.exceptionHandler
        //       ).map { preparedQuery =>
        //         preparedQuery
        //           .execute()
        //           .evalTap(n => info(user, s"Subscription event: ${n.printWith(Printer.spaces2)}"))
        //           .map(_.asRight[Throwable])
        //           .recover { case NonFatal(error) => error.asLeft[Json] }
        //       }
        //     }
        //   }
        // }

      // }

    }
}
