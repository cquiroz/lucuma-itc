// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.syntax.*
import io.circe.syntax.*
import lucuma.itc.ItcVersions
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

/**
 * Client for calling the ITC on the JVM.
 */
trait ItcClient[F[_]] {
  def spectroscopy(
    input:    SpectroscopyIntegrationTimeInput,
    useCache: Boolean = true
  ): F[IntegrationTimeResult]

  def imaging(
    input:    ImagingIntegrationTimeInput,
    useCache: Boolean = true
  ): F[IntegrationTimeResult]

  def spectroscopyGraphs(
    input:    SpectroscopyGraphsInput,
    useCache: Boolean = true
  ): F[SpectroscopyGraphsResult]

  def spectroscopyIntegrationTimeAndGraphs(
    input:    SpectroscopyIntegrationTimeAndGraphsInput,
    useCache: Boolean = true
  ): F[SpectroscopyIntegrationTimeAndGraphsResult]

  def versions: F[ItcVersions]
}

object ItcClient {
  def apply[F[_]](using ev: ItcClient[F]): ItcClient[F] = ev

  def create[F[_]: Async: Logger](
    uri:    Uri,
    client: Client[F]
  ): F[ItcClient[F]] =
    for
      specCache         <- ItcCache.simple[F, SpectroscopyIntegrationTimeInput, IntegrationTimeResult]
      imgCache          <- ItcCache.simple[F, ImagingIntegrationTimeInput, IntegrationTimeResult]
      graphCache        <-
        ItcCache.simple[F, SpectroscopyGraphsInput, SpectroscopyGraphsResult]
      timeAndGraphCache <-
        ItcCache.simple[F,
                        SpectroscopyIntegrationTimeAndGraphsInput,
                        SpectroscopyIntegrationTimeAndGraphsResult
        ]
      http              <- Http4sHttpClient.of[F, Unit](uri)(Async[F], Http4sHttpBackend(client), Logger[F])
    yield new ItcClient[F] {

      override def spectroscopy(
        input:    SpectroscopyIntegrationTimeInput,
        useCache: Boolean = true
      ): F[IntegrationTimeResult] = {

        val callOut: F[IntegrationTimeResult] =
          http
            .request(SpectroscopyIntegrationTime)
            .withInput(input)
            .raiseGraphQLErrorsOnNoData

        for
          _ <- Logger[F].debug(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) specCache.getOrCalcF(input)(callOut)
               else callOut.flatTap(specCache.put(input))
          _ <- Logger[F].debug(s"ITC Result:\n$v")
        yield v
      }

      override def imaging(
        input:    ImagingIntegrationTimeInput,
        useCache: Boolean = true
      ): F[IntegrationTimeResult] = {

        val callOut: F[IntegrationTimeResult] =
          http
            .request(ImagingIntegrationTime)
            .withInput(input)
            .raiseGraphQLErrorsOnNoData

        for
          _ <- Logger[F].debug(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) imgCache.getOrCalcF(input)(callOut)
               else callOut.flatTap(imgCache.put(input))
          _ <- Logger[F].debug(s"ITC Result:\n$v")
        yield v
      }

      override val versions: F[ItcVersions] =
        http.request(VersionsQuery).raiseGraphQLErrors

      def spectroscopyGraphs(
        input:    SpectroscopyGraphsInput,
        useCache: Boolean = true
      ): F[SpectroscopyGraphsResult] = {
        val callOut: F[SpectroscopyGraphsResult] =
          http
            .request(SpectroscopyGraphsQuery)
            .withInput(input)
            .raiseGraphQLErrorsOnNoData

        for
          _ <- Logger[F].debug(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) graphCache.getOrCalcF(input)(callOut)
               else callOut.flatTap(graphCache.put(input))
          _ <- Logger[F].debug(s"ITC Result:\n$v")
        yield v
      }

      def spectroscopyIntegrationTimeAndGraphs(
        input:    SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean = true
      ): F[SpectroscopyIntegrationTimeAndGraphsResult] = {
        val callOut: F[SpectroscopyIntegrationTimeAndGraphsResult] =
          http
            .request(SpectroscopyIntegrationTimeAndGraphsQuery)
            .withInput(input)
            .raiseGraphQLErrorsOnNoData

        for
          _ <- Logger[F].debug(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) timeAndGraphCache.getOrCalcF(input)(callOut)
               else callOut.flatTap(timeAndGraphCache.put(input))
          _ <- Logger[F].debug(s"ITC Result:\n$v")
        yield v
      }
    }

}
