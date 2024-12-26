// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import buildinfo.BuildInfo
import cats.Order.given
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.GalaxySpectrum.Spiral
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.BrightnessUnits.Brightness
import lucuma.core.math.BrightnessUnits.FluxDensityContinuum
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessUnits.LineFlux
import lucuma.core.math.BrightnessValue
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.TaggedUnit
import lucuma.core.math.units.KilometersPerSecond
import lucuma.core.math.units.VegaMagnitude
import lucuma.core.math.units.WattsPerMeter2
import lucuma.core.math.units.WattsPerMeter2Micrometer
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.EmissionLine
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.core.model.UnnormalizedSED.Galaxy
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.*
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.FinalSN
import lucuma.itc.GraphType
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.ItcVersions
import lucuma.itc.SeriesDataType
import lucuma.itc.SingleSN
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.service.ItcMapping.versionDateTimeFormatter
import lucuma.refined.*

import java.time.Instant
import scala.collection.immutable.SortedMap

class WiringSuite extends ClientSuite {
  val selected = IntegrationTime(
    TimeSpan.FromString.getOption("PT1S").get,
    PosInt.unsafeFrom(10),
    SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(10.0)),
    SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(11.0))
  )

  test("ItcClient spectroscopy basic wiring and sanity check") {
    spectroscopy(
      WiringSuite.SpectroscopyInput,
      IntegrationTimeResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft
              ).asRight
      ).asRight
    )
  }

  test("ItcClient imaging basic wiring and sanity check") {
    imaging(
      WiringSuite.ImagingInput,
      IntegrationTimeResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft
              ).asRight
      ).asRight
    )
  }

  test("ItcClient spectroscopy graph wiring and sanity check") {
    spectroscopyGraphs(
      WiringSuite.GraphInput,
      SpectroscopyGraphsResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismTargetGraphsResultOutcomes:
          NonEmptyChain.of(
            TargetGraphsResultOutcome:
              TargetGraphsResult(
                TargetGraphs(
                  NonEmptyChain.of(
                    ItcCcd(
                      1,
                      1,
                      2,
                      2,
                      Wavelength.fromIntNanometers(1001).get,
                      Wavelength.fromIntNanometers(1001).get,
                      3,
                      4,
                      5,
                      Nil
                    )
                  ),
                  NonEmptyChain.of(
                    GraphResult(
                      GraphType.S2NGraph,
                      List(
                        SeriesResult(
                          "title",
                          SeriesDataType.FinalS2NData,
                          List(1000.0, 1001.0),
                          ItcAxis(1, 2, 1, 2, 2).some,
                          ItcAxis(1000.0, 1001.0, 1000, 1001, 2).some
                        )
                      )
                    )
                  ),
                  FinalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
                  SignalToNoise.fromInt(1001).map(FinalSN(_)),
                  SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
                  SignalToNoise.fromInt(1002).map(SingleSN(_))
                ),
                Band.R
              ).asRight
          )
      ).asRight
    )
  }

  test("ItcClient spectroscopy with emission lines basic wiring and sanity check") {
    spectroscopyEmissionLines(
      WiringSuite.SpectroscopyEmissionLinesInput,
      IntegrationTimeResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Wavelength.unsafeFromIntPicometers(650000).asRight
              ).asRight
      ).asRight
    )
  }
}

object WiringSuite {

  val SpectroscopyInput: SpectroscopyIntegrationTimeInput =
    SpectroscopyIntegrationTimeInput(
      SpectroscopyIntegrationTimeParameters(
        Wavelength.Min,
        SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
        ConstraintSet(
          ImageQuality.PointOne,
          CloudExtinction.PointOne,
          SkyBackground.Darkest,
          WaterVapor.VeryDry,
          AirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val ImagingInput: ImagingIntegrationTimeInput =
    ImagingIntegrationTimeInput(
      ImagingIntegrationTimeParameters(
        Wavelength.Min,
        SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
        ConstraintSet(
          ImageQuality.PointOne,
          CloudExtinction.PointOne,
          SkyBackground.Darkest,
          WaterVapor.VeryDry,
          AirMass.Default
        ),
        InstrumentMode.GmosNorthImaging(
          GmosNorthFilter.GPrime,
          none
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val GraphInput: SpectroscopyGraphsInput =
    SpectroscopyGraphsInput(
      SpectroscopyGraphParameters(
        Wavelength.Min,
        TimeSpan.fromSeconds(1).get,
        PosInt.unsafeFrom(5),
        ConstraintSet(
          ImageQuality.PointOne,
          CloudExtinction.PointOne,
          SkyBackground.Darkest,
          WaterVapor.VeryDry,
          AirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        ),
        Some(SignificantFiguresInput(2.refined, 2.refined, 2.refined))
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val SpectroscopyEmissionLinesInput: SpectroscopyIntegrationTimeInput =
    SpectroscopyIntegrationTimeInput(
      SpectroscopyIntegrationTimeParameters(
        Wavelength.Min,
        SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
        ConstraintSet(
          ImageQuality.PointOne,
          CloudExtinction.PointOne,
          SkyBackground.Darkest,
          WaterVapor.VeryDry,
          AirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            EmissionLines[Integrated](
              SortedMap(
                Wavelength.unsafeFromIntPicometers(650000) ->
                  EmissionLine(
                    LineWidthValue.unsafeFrom(BigDecimal(1.0)).withUnit[KilometersPerSecond],
                    Measure(
                      LineFluxValue.unsafeFrom(BigDecimal(0.5)),
                      TaggedUnit[WattsPerMeter2, LineFlux[Integrated]].unit
                    ).tag
                  )
              ),
              Measure(
                FluxDensityContinuumValue.unsafeFrom(BigDecimal(0.5)),
                TaggedUnit[WattsPerMeter2Micrometer, FluxDensityContinuum[Integrated]].unit
              ).tag
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )
}
