// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.GraphType
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcCcd
import lucuma.itc.ItcGraph
import lucuma.itc.ItcGraphGroup
import lucuma.itc.ItcObservingConditions
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import lucuma.refined.*

object MockItc extends Itc[IO]:

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one:
        IntegrationTime(TimeSpan.fromSeconds(1).get,
                        10.refined,
                        SignalToNoise.fromInt(10).get,
                        SignalToNoise.fromInt(11).get
        )
      ,
      Band.R.asLeft
    ).pure[IO]

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    TargetGraphsCalcResult(
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
        ItcGraphGroup(
          NonEmptyChain.of(
            ItcGraph(
              GraphType.S2NGraph,
              List(
                ItcSeries("title", SeriesDataType.FinalS2NData, List((1.0, 1000.0), (2.0, 1001.0)))
              )
            )
          )
        )
      ),
      FinalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
      SignalToNoise.fromInt(1001).map(FinalSN.apply(_)),
      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_)),
      Band.R.asLeft
    )
      .pure[IO]

object MockImagingItc extends Itc[IO]:

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.of(
        IntegrationTime(TimeSpan.fromSeconds(1).get,
                        10.refined,
                        SignalToNoise.fromInt(10).get,
                        SignalToNoise.fromInt(20).get
        ),
        IntegrationTime(TimeSpan.fromSeconds(2).get,
                        5.refined,
                        SignalToNoise.fromInt(20).get,
                        SignalToNoise.fromInt(21).get
        )
      ),
      Band.R.asLeft
    ).pure[IO]

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    TargetGraphsCalcResult(
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
        ItcGraphGroup(
          NonEmptyChain.of(
            ItcGraph(
              GraphType.S2NGraph,
              List(
                ItcSeries("title", SeriesDataType.FinalS2NData, List((1.0, 1000.0), (2.0, 1001.0)))
              )
            )
          )
        )
      ),
      FinalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
      SignalToNoise.fromInt(1001).map(FinalSN.apply(_)),
      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_)),
      Band.R.asLeft
    )
      .pure[IO]

object EmissionLineMockItc extends Itc[IO]:

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one:
        IntegrationTime(TimeSpan.fromSeconds(1).get,
                        10.refined,
                        SignalToNoise.fromInt(10).get,
                        SignalToNoise.fromInt(11).get
        )
      ,
      Wavelength.unsafeFromIntPicometers(650000).asRight
    ).pure[IO]

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    IO.raiseError(CalculationError("Not implemented"))

object FailingMockItc extends Itc[IO]:

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    IO.raiseError(CalculationError("A calculation error"))

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    TargetGraphsCalcResult(
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
        ItcGraphGroup(
          NonEmptyChain.of(
            ItcGraph(
              GraphType.S2NGraph,
              List(
                ItcSeries("title", SeriesDataType.FinalS2NData, List((1.0, 1000.0), (2.0, 1001.0)))
              )
            )
          )
        )
      ),
      FinalSN(SignalToNoise.unsafeFromBigDecimalExact(1000.0)),
      SignalToNoise.fromInt(1001).map(FinalSN.apply(_)),
      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_)),
      Band.R.asLeft
    )
      .pure[IO]
