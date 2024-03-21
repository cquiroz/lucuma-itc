// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import cats.Hash
import cats.derived.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.GmosNImagingParams
import lucuma.itc.GmosNSpectroscopyParams
import lucuma.itc.GmosSImagingParams
import lucuma.itc.GmosSSpectroscopyParams
import lucuma.itc.encoders.given
import lucuma.itc.search.ItcObservationDetails.AnalysisMethod
import lucuma.itc.search.hashes.given
import lucuma.itc.search.syntax.*
import spire.math.Interval
import spire.math.Rational

case class GmosNorthFpuParam(
  builtin: GmosNorthFpu
) derives Encoder.AsObject {
  def isIfu: Boolean            = builtin.isGNIfu
  def effectiveSlitWidth: Angle = builtin.effectiveSlitWidth
}

case class GmosSouthFpuParam(
  builtin: GmosSouthFpu
) derives Encoder.AsObject {
  def isIfu: Boolean            = builtin.isGSIfu
  def effectiveSlitWidth: Angle = builtin.effectiveSlitWidth
}

sealed trait ObservingMode {
  def instrument: Instrument
  def analysisMethod: ItcObservationDetails.AnalysisMethod
}

object ObservingMode {
  given Encoder[ObservingMode] = Encoder.instance {
    case spec: SpectroscopyMode => spec.asJson
    case img: ImagingMode       => img.asJson
  }

  sealed trait SpectroscopyMode extends ObservingMode derives Hash {
    def λ: Wavelength
    def resolution: Rational
    def coverage: Interval[Wavelength]
  }

  object SpectroscopyMode {
    given Encoder[ObservingMode.SpectroscopyMode] = Encoder.instance {
      case gn: GmosNorth => gn.asJson
      case gs: GmosSouth => gs.asJson
    }

    sealed trait GmosSpectroscopy extends SpectroscopyMode derives Hash {
      def isIfu: Boolean

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        if (isIfu)
          ItcObservationDetails.AnalysisMethod.Ifu.Single(
            skyFibres = 250,
            offset = 5.0
          )
        else
          ItcObservationDetails.AnalysisMethod.Aperture.Auto(
            skyAperture = 5.0
          )
    }

    case class GmosNorth(
      λ:         Wavelength,
      disperser: GmosNorthGrating,
      fpu:       GmosNorthFpuParam,
      filter:    Option[GmosNorthFilter],
      ccdMode:   Option[GmosCcdMode],
      roi:       Option[GmosRoi]
    ) extends GmosSpectroscopy
        derives Hash {
      val isIfu = fpu.isIfu

      val instrument: Instrument =
        Instrument.GmosNorth

      def resolution: Rational =
        disperser.resolution(λ, fpu.effectiveSlitWidth)

      def coverage: Interval[Wavelength] =
        filter.foldLeft(disperser.simultaneousCoverage.centeredAt(λ).toInterval)((a, b) =>
          a.intersect(b.coverageGN)
        )
    }

    object GmosNorth:
      given Encoder[GmosNorth] = a =>
        Json.obj(
          ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
          ("resolution", Json.fromInt(a.resolution.toInt)),
          ("params", GmosNSpectroscopyParams(a.disperser, a.fpu, a.filter).asJson),
          ("wavelength", a.λ.asJson)
        )

    case class GmosSouth(
      λ:         Wavelength,
      disperser: GmosSouthGrating,
      fpu:       GmosSouthFpuParam,
      filter:    Option[GmosSouthFilter],
      ccdMode:   Option[GmosCcdMode],
      roi:       Option[GmosRoi]
    ) extends GmosSpectroscopy
        derives Hash {
      val isIfu = fpu.isIfu

      val instrument: Instrument =
        Instrument.GmosSouth

      def resolution: Rational =
        disperser.resolution(λ, fpu.effectiveSlitWidth)

      def coverage: Interval[Wavelength] =
        filter.foldLeft(disperser.simultaneousCoverage.centeredAt(λ).toInterval)((a, b) =>
          a.intersect(b.coverageGS)
        )
    }

    object GmosSouth:
      given Encoder[GmosSouth] = a =>
        Json.obj(
          ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
          ("resolution", Json.fromInt(a.resolution.toInt)),
          ("params", GmosSSpectroscopyParams(a.disperser, a.fpu, a.filter).asJson),
          ("wavelength", a.λ.asJson)
        )

  }

  sealed trait ImagingMode extends ObservingMode derives Hash {
    def λ: Wavelength
  }

  object ImagingMode {
    given Encoder[ObservingMode.ImagingMode] = Encoder.instance {
      case gn: GmosNorth => gn.asJson
      case gs: GmosSouth => gs.asJson
    }

    sealed trait GmosImaging extends ImagingMode derives Hash {

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 5.0
        )
    }

    case class GmosNorth(
      λ:       Wavelength,
      filter:  GmosNorthFilter,
      ccdMode: Option[GmosCcdMode]
    ) extends GmosImaging {

      val instrument: Instrument =
        Instrument.GmosNorth

    }

    object GmosNorth:
      given Encoder[GmosNorth] = a =>
        Json.obj(
          ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
          ("params", GmosNImagingParams(a.filter).asJson),
          ("wavelength", a.λ.asJson)
        )

    case class GmosSouth(
      λ:       Wavelength,
      filter:  GmosSouthFilter,
      ccdMode: Option[GmosCcdMode]
    ) extends GmosImaging {
      val instrument: Instrument =
        Instrument.GmosSouth
    }

    object GmosSouth:
      given Encoder[GmosSouth] = a =>
        Json.obj(
          ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
          ("params", GmosSImagingParams(a.filter).asJson),
          ("wavelength", a.λ.asJson)
        )
  }

}
