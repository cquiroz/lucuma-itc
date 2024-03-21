// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.itc.ItcObservingConditions
import lucuma.itc.search.ItcObservationDetails
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile

import scala.concurrent.duration.FiniteDuration

enum ItcWavefrontSensor(val ocs2Tag: String):
  case PWFS  extends ItcWavefrontSensor("PWFS")
  case OIWFS extends ItcWavefrontSensor("OIWFS")

case class ItcTelescopeDetails(wfs: ItcWavefrontSensor)

case class ItcSourceDefinition(
  profile:  SourceProfile,
  normBand: Band,
  redshift: Redshift
)

object ItcSourceDefinition:

  def fromTargetProfile(p: TargetProfile): ItcSourceDefinition =
    ItcSourceDefinition(
      p.sourceProfile,
      p.band,
      p.redshift
    )

case class ItcParameters(
  source:      ItcSourceDefinition,
  observation: ItcObservationDetails,
  conditions:  ItcObservingConditions,
  telescope:   ItcTelescopeDetails,
  instrument:  ItcInstrumentDetails
)

case class ItcInstrumentDetails(mode: ObservingMode)

object ItcInstrumentDetails:
  def fromObservingMode(mode: ObservingMode): ItcInstrumentDetails =
    apply(mode)

/** Convert model types into OCS2 ITC-compatible types for a spectroscopy request. */
def spectroscopyParams(
  targetProfile:    TargetProfile,
  observingMode:    ObservingMode,
  exposureDuration: FiniteDuration,
  conditions:       ItcObservingConditions,
  exposures:        Int
): ItcParameters =
  ItcParameters(
    source = ItcSourceDefinition.fromTargetProfile(targetProfile),
    observation = ItcObservationDetails(
      calculationMethod = ItcObservationDetails.CalculationMethod.SignalToNoise.Spectroscopy(
        exposures = exposures,
        coadds = None,
        exposureDuration = exposureDuration,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
      analysisMethod = observingMode.analysisMethod
    ),
    conditions = conditions,
    telescope = ItcTelescopeDetails(
      wfs = ItcWavefrontSensor.OIWFS
    ),
    instrument = ItcInstrumentDetails.fromObservingMode(observingMode)
  )

def spectroscopyWithSNAtParams(
  targetProfile: TargetProfile,
  observingMode: ObservingMode,
  conditions:    ItcObservingConditions,
  sigma:         SignalToNoise,
  wavelength:    Wavelength
): ItcParameters =
  ItcParameters(
    source = ItcSourceDefinition.fromTargetProfile(targetProfile),
    observation = ItcObservationDetails(
      calculationMethod =
        ItcObservationDetails.CalculationMethod.SignalToNoise.SpectroscopyWithSNAt(
          sigma = sigma.toBigDecimal.toDouble,
          coadds = None,
          wavelength = wavelength,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0
        ),
      analysisMethod = observingMode.analysisMethod
    ),
    conditions = conditions,
    telescope = ItcTelescopeDetails(
      wfs = ItcWavefrontSensor.OIWFS
    ),
    instrument = ItcInstrumentDetails.fromObservingMode(observingMode)
  )

def imagingParams(
  targetProfile: TargetProfile,
  observingMode: ObservingMode,
  conditions:    ItcObservingConditions,
  sigma:         SignalToNoise
): ItcParameters =
  ItcParameters(
    source = ItcSourceDefinition.fromTargetProfile(targetProfile),
    observation = ItcObservationDetails(
      calculationMethod = ItcObservationDetails.CalculationMethod.IntegrationTime.ImagingExp(
        sigma = sigma.toBigDecimal.toDouble,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
      analysisMethod = observingMode.analysisMethod
    ),
    conditions = conditions,
    telescope = ItcTelescopeDetails(
      wfs = ItcWavefrontSensor.OIWFS
    ),
    instrument = ItcInstrumentDetails.fromObservingMode(observingMode)
  )
