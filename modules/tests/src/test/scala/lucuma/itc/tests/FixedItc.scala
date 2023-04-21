// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.applicative.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.itc.ChartType
import lucuma.itc.IntegrationTimeResult
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcChartGroup
import lucuma.itc.ItcObservingConditions
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.SignalToNoiseCalculation
import lucuma.itc.*
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.refined.*

import scala.concurrent.duration._

object FixedItc extends Itc[IO] with SignalToNoiseCalculation[IO] {

  override def calculateExposureTime(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    signalToNoise:   SignalToNoise,
    signalToNoiseAt: Option[Wavelength]
  ): IO[IntegrationTimeResult] =
    IntegrationTimeResult
      .ExposureTimeSuccess(1.seconds, 10.refined, SignalToNoise.fromInt(10).get)
      .pure[IO]

  override def calculateGraph(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  NonNegDuration,
    exposures:     PosLong
  ): IO[GraphResult] =
    GraphResult(
      "1",
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
      )
    )
      .pure[IO]

}
