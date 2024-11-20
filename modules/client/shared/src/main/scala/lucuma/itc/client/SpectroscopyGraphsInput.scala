// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan
import lucuma.itc.client.json.given
import lucuma.itc.client.json.syntax.*
import lucuma.itc.encoders.given

case class SpectroscopyGraphParameters(
  wavelength:         Wavelength,
  exposureTime:       TimeSpan,
  exposureCount:      PosInt,
  constraints:        ConstraintSet,
  mode:               InstrumentMode,
  significantFigures: Option[SignificantFiguresInput]
) derives Eq

case class SpectroscopyGraphsInput(
  parameters: SpectroscopyGraphParameters,
  asterism:   NonEmptyList[TargetInput]
) derives Eq:
  export parameters.*

object SpectroscopyGraphsInput {
  given Encoder[TimeSpan] = _.toMicroseconds.asJson

  given Encoder.AsObject[SpectroscopyGraphsInput] = a =>
    JsonObject(
      "wavelength"         -> Json.obj("picometers" -> a.wavelength.toPicometers.value.asJson),
      "exposureTime"       -> Json.obj("microseconds" -> a.exposureTime.asJson),
      "exposureCount"      -> a.exposureCount.value.asJson,
      "asterism"           -> a.asterism.asJson,
      "constraints"        -> a.constraints.asJson,
      "mode"               -> a.mode.asJson,
      "significantFigures" -> a.significantFigures.asJson
    )
}
