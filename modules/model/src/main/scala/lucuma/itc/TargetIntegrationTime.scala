// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.Order
import cats.derived.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec.given
import lucuma.core.enums.Band
import lucuma.core.math.Wavelength
import lucuma.itc.encoders.given

case class TargetIntegrationTime(
  times:      Zipper[IntegrationTime],
  bandOrLine: Either[Band, Wavelength]
) derives Eq:
  def focusIndex(index: Int): Option[TargetIntegrationTime] =
    times
      .focusIndex(index)
      .map: newTimes =>
        copy(times = newTimes)

object TargetIntegrationTime:
  given Encoder[TargetIntegrationTime] = t =>
    Json
      .obj(
        "band"         -> t.bandOrLine.left.toOption.asJson,
        "emissionLine" -> t.bandOrLine.toOption.asJson
      )
      .deepMerge(t.times.asJson)

  val exposureTimeOrder: Order[TargetIntegrationTime] =
    given Order[IntegrationTime] = IntegrationTime.exposureTimeOrder
    Order.by(_.times.focus)
