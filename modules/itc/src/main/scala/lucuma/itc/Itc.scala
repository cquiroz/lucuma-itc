// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import lucuma.itc.search._

import scala.concurrent.duration.FiniteDuration

trait Itc[F[_]] {

  /**
   * Compute the exposure time and number required to achieve the desired signal-to-noise under
   * average conditions.
   */
  def calculate(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: BigDecimal
  ): F[Itc.Result]

}

object Itc {

  def apply[F[_]](implicit ev: Itc[F]): ev.type = ev

  sealed trait Result extends Product with Serializable
  object Result {

    final case class Success(
      exposureTime:  FiniteDuration,
      exposures:     Int,
      signalToNoise: BigDecimal
    ) extends Result

    /** Object is too bright to be observed in the specified mode. */
    final case class SourceTooBright(msg: String) extends Result

    /** Generic calculation error */
    final case class CalculationError(msg: String) extends Result
  }

}
