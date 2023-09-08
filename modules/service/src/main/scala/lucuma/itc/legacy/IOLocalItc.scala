// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.effect.Async
import cats.effect.kernel.syntax.all.*
import cats.syntax.all.*
import lucuma.itc.UpstreamException
import lucuma.itc.legacy

/**
 * Wraps local calls to ITC to ensure fairness
 */
case class FLocalItc[F[_]: Async](itcLocal: LocalItc):
  private val F = Async[F]

  def calculateCharts(jsonParams: String): F[GraphsRemoteResult] =
    (F.cede *> F.delay(itcLocal.calculateCharts(jsonParams)).guarantee(F.cede)).flatMap {
      case Right(result) => F.pure(result)
      case Left(msg)     => F.raiseError(new UpstreamException(msg))
    }

  def calculateExposureTime(jsonParams: String): F[ExposureTimeRemoteResult] =
    (F.cede *> F.delay(itcLocal.calculateExposureTime(jsonParams)).guarantee(F.cede)).flatMap {
      case Right(result) => F.pure(result)
      case Left(msg)     => F.raiseError(new UpstreamException(msg))
    }
