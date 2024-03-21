// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Hash
import cats.derived.*
import cats.syntax.all.*
import io.circe.*
import lucuma.core.enums.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.itc.search.*
import lucuma.itc.search.ObservingMode.SpectroscopyMode.*

sealed trait SpectroscopyParams

case class GmosNSpectroscopyParams(
  grating: GmosNorthGrating,
  fpu:     GmosNorthFpuParam,
  filter:  Option[GmosNorthFilter]
) extends SpectroscopyParams
    derives Encoder.AsObject

case class GmosSSpectroscopyParams(
  grating: GmosSouthGrating,
  fpu:     GmosSouthFpuParam,
  filter:  Option[GmosSouthFilter]
) extends SpectroscopyParams
    derives Encoder.AsObject

sealed trait ImagingParams

case class GmosNImagingParams(filter: GmosNorthFilter) extends ImagingParams
    derives Encoder.AsObject

case class GmosSImagingParams(filter: GmosSouthFilter) extends ImagingParams
    derives Encoder.AsObject

case class ItcObservingConditions(
  iq:      ImageQuality,
  cc:      CloudExtinction,
  wv:      WaterVapor,
  sb:      SkyBackground,
  airmass: Double
) derives Hash

object ItcObservingConditions {
  val AirMassBuckets = Vector(BigDecimal(1.2), BigDecimal(1.5), BigDecimal(2.0))

  def fromConstraints(constraints: ConstraintSet): Either[String, ItcObservingConditions] =
    val airmass = constraints.elevationRange match {
      case ElevationRange.AirMass(min, max) if max.value >= min.value   =>
        AirMassBuckets.find(max.value <= _).getOrElse(AirMassBuckets.last).asRight
      case ElevationRange.AirMass(min, max)                             =>
        Left("Maximum airmass must be greater than minimum airmass")
      case ElevationRange.HourAngle(min, max) if max.value >= min.value =>
        max.value.asRight
      case ElevationRange.HourAngle(min, max)                           =>
        Left(s"Hour Angle max value $max must be more than the min value $min")
    }
    airmass.map(a =>
      ItcObservingConditions(constraints.imageQuality,
                             constraints.cloudExtinction,
                             constraints.waterVapor,
                             constraints.skyBackground,
                             a.toDouble
      )
    )
}
