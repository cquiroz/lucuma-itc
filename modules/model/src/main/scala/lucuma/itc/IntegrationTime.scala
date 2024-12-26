// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.Order
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.*
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.itc.encoders.given
import spire.implicits.*

case class IntegrationTime(
  exposureTime:        TimeSpan,
  exposureCount:       PosInt,
  totalSignalToNoise:  SignalToNoise,
  singleSignalToNoise: SignalToNoise
) derives Eq,
      Encoder.AsObject

object IntegrationTime:
  // The brightest target will be the one with the smallest exposure time.
  // We break ties by exposure count.
  val exposureTimeOrder: Order[IntegrationTime] =
    Order.by(it => (it.exposureTime, it.exposureCount))
