// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.Band
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbConstraintSet
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbIntegrationTimeInput {

  import ArbConstraintSet.*
  import ArbEnumerated.given
  import ArbInstrumentMode.given
  import ArbRadialVelocity.given
  import ArbSignalToNoise.given
  import ArbSourceProfile.given
  import ArbWavelength.given

  def genSpectroscopyIntegrationTimeInput(
    im: InstrumentMode
  ): Gen[SpectroscopyIntegrationTimeInput] =
    for {
      w   <- arbitrary[Wavelength]
      s2n <- arbitrary[SignalToNoise]
      sat <- arbitrary[Option[Wavelength]]
      sp  <- arbitrary[SourceProfile]
      b   <- arbitrary[Band]
      rv  <- arbitrary[RadialVelocity]
      cs  <- arbitrary[ConstraintSet]
      im  <- arbitrary[InstrumentMode]
    } yield SpectroscopyIntegrationTimeInput(
      w,
      s2n,
      sat,
      sp,
      b,
      rv,
      cs,
      im
    )

  given Arbitrary[SpectroscopyIntegrationTimeInput] =
    Arbitrary {
      for {
        im <- arbitrary[InstrumentMode]
        sm <- genSpectroscopyIntegrationTimeInput(im)
      } yield sm
    }

  given Cogen[SpectroscopyIntegrationTimeInput] =
    Cogen[
      (
        Wavelength,
        SignalToNoise,
        Option[Wavelength],
        SourceProfile,
        Band,
        RadialVelocity,
        ConstraintSet,
        InstrumentMode
      )
    ].contramap { a =>
      (
        a.wavelength,
        a.signalToNoise,
        a.signalToNoiseAt,
        a.sourceProfile,
        a.band,
        a.radialVelocity,
        a.constraints,
        a.mode
      )
    }

  def genImagingIntegrationTimeInput(
    im: InstrumentMode
  ): Gen[ImagingIntegrationTimeInput] =
    for {
      w   <- arbitrary[Wavelength]
      s2n <- arbitrary[SignalToNoise]
      sp  <- arbitrary[SourceProfile]
      b   <- arbitrary[Band]
      rv  <- arbitrary[RadialVelocity]
      cs  <- arbitrary[ConstraintSet]
      im  <- arbitrary[InstrumentMode]
    } yield ImagingIntegrationTimeInput(
      w,
      s2n,
      sp,
      b,
      rv,
      cs,
      im
    )

  given Arbitrary[ImagingIntegrationTimeInput] =
    Arbitrary {
      for {
        im <- arbitrary[InstrumentMode]
        sm <- genImagingIntegrationTimeInput(im)
      } yield sm
    }

  given Cogen[ImagingIntegrationTimeInput] =
    Cogen[
      (
        Wavelength,
        SignalToNoise,
        SourceProfile,
        Band,
        RadialVelocity,
        ConstraintSet,
        InstrumentMode
      )
    ].contramap { a =>
      (
        a.wavelength,
        a.signalToNoise,
        a.sourceProfile,
        a.band,
        a.radialVelocity,
        a.constraints,
        a.mode
      )
    }
}

object ArbIntegrationTimeInput extends ArbIntegrationTimeInput
