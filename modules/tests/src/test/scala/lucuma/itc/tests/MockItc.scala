// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.applicative.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.ChartType
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcChartGroup
import lucuma.itc.ItcObservingConditions
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.SignalToNoiseCalculation
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.refined.*

object MockItc extends Itc[IO] with SignalToNoiseCalculation[IO]:

  override def calculateIntegrationTime(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    signalToNoise:   SignalToNoise,
    signalToNoiseAt: Option[Wavelength]
  ): IO[NonEmptyList[IntegrationTime]] =
    NonEmptyList
      .of(IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined, SignalToNoise.fromInt(10).get))
      .pure[IO]

  override def calculateGraph(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    exposureTime:    TimeSpan,
    exposures:       PosInt,
    signalToNoiseAt: Option[Wavelength]
  ): IO[GraphResult] =
    GraphResult(
      NonEmptyList.of(
        ItcCcd(1,
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
      NonEmptyList.of(
        ItcChartGroup(
          NonEmptyList.of(
            ItcChart(
              ChartType.S2NChart,
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
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_))
    )
      .pure[IO]

object MockImagingItc extends Itc[IO] with SignalToNoiseCalculation[IO]:

  override def calculateIntegrationTime(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    signalToNoise:   SignalToNoise,
    signalToNoiseAt: Option[Wavelength]
  ): IO[NonEmptyList[IntegrationTime]] =
    NonEmptyList
      .of(
        IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined, SignalToNoise.fromInt(10).get),
        IntegrationTime(TimeSpan.fromSeconds(2).get, 5.refined, SignalToNoise.fromInt(20).get)
      )
      .pure[IO]

  override def calculateGraph(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    exposureTime:    TimeSpan,
    exposures:       PosInt,
    signalToNoiseAt: Option[Wavelength]
  ): IO[GraphResult] =
    GraphResult(
      NonEmptyList.of(
        ItcCcd(1,
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
      NonEmptyList.of(
        ItcChartGroup(
          NonEmptyList.of(
            ItcChart(
              ChartType.S2NChart,
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
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_))
    )
      .pure[IO]

object FailingMockItc extends Itc[IO] with SignalToNoiseCalculation[IO]:

  override def calculateIntegrationTime(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    signalToNoise:   SignalToNoise,
    signalToNoiseAt: Option[Wavelength]
  ): IO[NonEmptyList[IntegrationTime]] =
    IO.raiseError(CalculationError("A calculation error"))

  override def calculateGraph(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    exposureTime:    TimeSpan,
    exposures:       PosInt,
    signalToNoiseAt: Option[Wavelength]
  ): IO[GraphResult] =
    GraphResult(
      NonEmptyList.of(
        ItcCcd(1,
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
      NonEmptyList.of(
        ItcChartGroup(
          NonEmptyList.of(
            ItcChart(
              ChartType.S2NChart,
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
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_))
    )
      .pure[IO]
