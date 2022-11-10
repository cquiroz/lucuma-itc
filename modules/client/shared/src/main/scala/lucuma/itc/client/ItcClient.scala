// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Applicative
import cats.ApplicativeError
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import clue.TransactionalClient
import clue.http4s.Http4sBackend
import io.circe.syntax.*
import lucuma.core.model.Observation
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.Logger

/**
 * Client for calling the ITC on the JVM.
 */
trait ItcClient[F[_]] {

  // TODO: chart

  def spectroscopy(
    input:    SpectroscopyModeInput,
    useCache: Boolean = true
  ): F[SpectroscopyResult]

  def versions: F[ItcVersions]

}

object ItcClient {

  def create[F[_]: Async: Logger](
    uri:    Uri,
    client: Client[F]
  ): Resource[F, ItcClient[F]] =
    for {
      cache <- Resource.eval(ItcCache.simple[F, SpectroscopyModeInput, SpectroscopyResult])
      http  <- Resource.eval(
                 TransactionalClient.of[F, Unit](uri)(Async[F], Http4sBackend(client), Logger[F])
               )
    } yield new ItcClient[F] {
      override def spectroscopy(
        input:    SpectroscopyModeInput,
        useCache: Boolean = true
      ): F[SpectroscopyResult] = {

        val callOut: F[SpectroscopyResult] =
          for {
            r  <- http.request(SpectroscopyQuery)(input)
            rʹ <- ApplicativeError.liftFromOption[F](
                    r.headOption,
                    new RuntimeException("No results returned by ITC.")
                  )
          } yield rʹ

        for {
          _ <- Logger[F].info(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) cache.getOrCalcF(input)(callOut)
               else callOut.flatTap(cache.put(input))
          _ <- Logger[F].info(s"ITC Result:\n$v")
        } yield v
      }

      override val versions: F[ItcVersions] =
        http.request(VersionsQuery)

    }

}
