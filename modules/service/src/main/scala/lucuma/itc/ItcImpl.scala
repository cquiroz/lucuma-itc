// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import algebra.instances.all.given
import cats.*
import cats.data.NonEmptyChain
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.legacy.ExposureTimeRemoteResult
import lucuma.itc.legacy.FLocalItc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import lucuma.refined.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.math.*

trait SignalToNoiseCalculation[F[_]: Applicative] { this: Itc[F] =>
  def calculateSignalToNoise(
    graph:           NonEmptyChain[ItcGraphGroup],
    signalToNoiseAt: Option[Wavelength]
  ): F[SNCalcResult] =
    (for {
      s2nGraph     <- graph.flatMap(_.graphs).find(_.graphType === GraphType.S2NGraph)
      finalS2NData <- s2nGraph.series
                        .filter(_.seriesType === SeriesDataType.FinalS2NData)
                        .map(_.data)
                        .flatten
                        .some
    } yield {
      def resultFromDouble(d: Double): SNCalcResult =
        SignalToNoise.FromBigDecimalRounding
          .getOption(d)
          .fold(SNCalcResult.CalculationError(s"Computed invalid signal-to-noise: $d")) { sn =>
            SNCalcResult.SNCalcSuccess(sn)
          }

      val sorted           = finalS2NData.sortBy(_._1)
      val sn: SNCalcResult = signalToNoiseAt
        .fold(resultFromDouble(sorted.maxBy(_._2)._2)) { w =>
          val nanos = Wavelength.decimalNanometers.reverseGet(w).doubleValue
          if (nanos < sorted.head._1) SNCalcResult.WavelengthAtBelowRange(w)
          else if (nanos > sorted.last._1) SNCalcResult.WavelengthAtAboveRange(w)
          else
            val sortedList = sorted.toList
            val index      = sortedList.indexWhere(_._1 >= nanos)
            sortedList.lift(index).fold(SNCalcResult.NoData()) { secondPoint =>
              val (w2, s2) = secondPoint
              if (w2 === nanos) {
                resultFromDouble(s2)
              } else {
                sortedList.lift(index - 1) match
                  case Some((w1, s1)) =>
                    // Linear interpolation
                    val sn = (s1 * (w2 - nanos) + s2 * (nanos - w1)) / (w2 - w1)
                    resultFromDouble(sn)
                  case _              =>
                    // We are checking the limits before, this shouldn't happen
                    SNCalcResult.NoData()
              }
            }
        }

      sn.pure[F]
    }).getOrElse(SNCalcResult.NoData().pure[F])

}

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type ExposureCount = Int

  def build[F[_]: MonadThrow: Logger: Trace](itcLocal: FLocalItc[F]): Itc[F] =
    new Itc[F] with SignalToNoiseCalculation[F] {
      val L = Logger[F]
      val T = Trace[F]

      def calculateIntegrationTime(
        target:          TargetData,
        band:            Band,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        signalToNoise:   SignalToNoise,
        signalToNoiseAt: Option[Wavelength]
      ): F[NonEmptyChain[IntegrationTime]] =
        T.span("calculate-exposure-time"):
          observingMode match
            case ObservingMode.SpectroscopyMode(_, _, _) =>
              signalToNoiseAt match
                // case None     =>
                //   spectroscopy(target, band, observingMode, constraints, signalToNoise, none)
                case Some(at) =>
                  spectroscopySNAt(target, band, observingMode, constraints, signalToNoise, at)
            case ObservingMode.ImagingMode.GmosNorth(_, _, _) |
                ObservingMode.ImagingMode.GmosSouth(_, _, _) =>
              imaging(target, band, observingMode, constraints, signalToNoise)

      def calculateGraph(
        target:          TargetData,
        band:            Band,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        exposureTime:    TimeSpan,
        exposureCount:   PosInt,
        signalToNoiseAt: Option[Wavelength]
      ): F[TargetGraphsCalcResult] =
        observingMode match
          case ObservingMode.SpectroscopyMode(_, _, _) =>
            spectroscopyGraph(
              target,
              band,
              observingMode,
              constraints,
              exposureTime.toMilliseconds.withUnit[Millisecond].toUnit[Second],
              exposureCount.value,
              signalToNoiseAt
            )
          case ObservingMode.ImagingMode.GmosNorth(_, _, _) |
              ObservingMode.ImagingMode.GmosSouth(_, _, _) =>
            MonadThrow[F].raiseError(
              new IllegalArgumentException("Imaging mode not supported for graph calculation")
            )

      private def itcGraph(
        target:           TargetData,
        band:             Band,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposureCount:    Int,
        level:            Option[NonNegInt] = none
      ): F[legacy.GraphsRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query") {
          val request =
            spectroscopyParams(
              target,
              band,
              observingMode,
              exposureDuration.value.toDouble.seconds,
              constraints,
              exposureCount
            ).asJson

          for {
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.exposureDuration" -> exposureDuration.value.toInt)
            _ <- T.put("itc.exposures" -> exposureCount)
            _ <- T.put("itc.level" -> level.map(_.value).orEmpty)
            r <- itcLocal.calculateGraphs(request.noSpaces)
          } yield r
        }

      private def itcWithSNAt(
        target:        TargetData,
        band:          Band,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        sigma:         SignalToNoise,
        wavelength:    Wavelength
      ): F[legacy.ExposureTimeRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query") {
          val request =
            spectroscopyWithSNAtParams(
              target,
              band,
              observingMode,
              constraints,
              sigma,
              wavelength
            ).asJson

          for {
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.sigma" -> sigma.toBigDecimal.toDouble)
            _ <- L.info(request.noSpaces) // Request to the legacy itc
            a <- itcLocal.calculateExposureTime(request.noSpaces)
          } yield a
        }

      private def spectroscopyGraph(
        target:           TargetData,
        band:             Band,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposureCount:    Int,
        signalToNoiseAt:  Option[Wavelength]
      ): F[TargetGraphsCalcResult] =
        itcGraph(target, band, observingMode, constraints, exposureDuration, exposureCount).map:
          r => TargetGraphsCalcResult.fromLegacy(r.ccds, r.groups, signalToNoiseAt)

      // /**
      //  * Compute the exposure time and number of exposures required to achieve the desired
      //  * signal-to-noise under the requested conditions. Only for spectroscopy modes
      //  */
      // private def spectroscopy(
      //   target:          TargetData,
      //   band:            Band,
      //   observingMode:   ObservingMode,
      //   constraints:     ItcObservingConditions,
      //   signalToNoise:   SignalToNoise,
      //   signalToNoiseAt: Option[Wavelength]
      // ): F[NonEmptyChain[IntegrationTime]] = {
      //   val startExpTime      = BigDecimal(1200.0).withUnit[Second]
      //   val numberOfExposures = 1
      //   val requestedSN       = signalToNoise
      //   val MaxIterations     = 10

      //   // This loops should be necessary only a few times but put a circuit breaker just in case
      //   def itcStep(
      //     nExp:       ExposureCount,
      //     oldNExp:    Int,
      //     expTime:    Quantity[BigDecimal, Second],
      //     oldExpTime: Quantity[BigDecimal, Second],
      //     snr:        SignalToNoise,
      //     maxTime:    Quantity[BigDecimal, Second],
      //     s:          legacy.GraphsRemoteResult,
      //     counter:    NonNegInt
      //   ): F[IntegrationTime] =
      //     val totalTime: Quantity[BigDecimal, Second] =
      //       if (snr === SignalToNoise.Min) TimeSpan.Max.toSeconds.withUnit[Second]
      //       else
      //         expTime * nExp
      //           .withUnit[1] * pow(requestedSN.toBigDecimal.toDouble / snr.toBigDecimal.toDouble, 2)
      //           .withUnit[1]

      //     val newNExp: BigDecimal = spire.math.ceil((totalTime / maxTime).value)

      //     val newExpTime: BigDecimal =
      //       spire.math.ceil((totalTime / newNExp.withUnit[1]).value)

      //     val next = NonNegInt.from(counter.value + 1).getOrElse(sys.error("Should not happen"))
      //     L.info(s"Total time: $totalTime maxTime: $maxTime") *>
      //       L.info(s"Exp time :$newExpTime s/Num exp $newNExp/iteration $counter") *> {
      //         if (
      //           nExp != oldNExp ||
      //           ((expTime - oldExpTime) > 1.withUnit[Second] || (oldExpTime - expTime) > 1
      //             .withUnit[Second]) &&
      //           counter.value < MaxIterations &&
      //           newExpTime < (pow(2, 63) - 1)
      //         ) {
      //           itcGraph(
      //             target,
      //             band,
      //             observingMode,
      //             constraints,
      //             newExpTime.withUnit[Second],
      //             newNExp.toInt,
      //             next.some
      //           )
      //             .flatMap { s =>
      //               L.debug(s"-> S/N: ${s.maxTotalSNRatio}") *>
      //                 calculateSignalToNoise(s.groups, signalToNoiseAt).flatMap {
      //                   case SNCalcResult.SNCalcSuccess(snr) =>
      //                     itcStep(
      //                       newNExp.toInt,
      //                       nExp,
      //                       newExpTime.withUnit[Second],
      //                       expTime,
      //                       snr,
      //                       maxTime,
      //                       s,
      //                       next
      //                     )
      //                   case r                               =>
      //                     MonadThrow[F].raiseError(CalculationError(r.toString))
      //                 }
      //             }
      //         } else {
      //           (SignalToNoise.FromBigDecimalRounding.getOption(s.maxTotalSNRatio),
      //            TimeSpan.fromSeconds(newExpTime),
      //            refineV[Positive](newNExp.toInt).toOption
      //           ) match {
      //             case (Some(sn), Some(expTime), Some(count)) =>
      //               IntegrationTime(expTime, count, sn).pure[F]
      //             case _                                      =>
      //               MonadThrow[F].raiseError(
      //                 CalculationError(
      //                   "Negative signal to noise or exposure count"
      //                 )
      //               )
      //           }
      //         }
      //       }
      //   end itcStep

      //   L.info(s"Desired S/N $signalToNoise") *>
      //     L.info(s"Target brightness $target at band $band") *>
      //     T.span("itc.calctime.spectroscopy") {

      //       itcGraph(
      //         target,
      //         band,
      //         observingMode,
      //         constraints,
      //         startExpTime,
      //         numberOfExposures,
      //         1.refined[NonNegative].some
      //       )
      //         .flatMap { r =>
      //           val wellHalfFilledSeconds =
      //             r.maxWellDepth / 2 / r.maxPeakPixelFlux * startExpTime.value
      //           L.info(
      //             s"Results CCD wellDepth: ${r.maxWellDepth}, peakPixelFlux: ${r.maxPeakPixelFlux}, totalSNRatio: ${r.maxTotalSNRatio} wellHalfFilledSeconds: $wellHalfFilledSeconds"
      //           ) *> {
      //             if (wellHalfFilledSeconds < 1.0) {
      //               MonadThrow[F].raiseError(SourceTooBright(wellHalfFilledSeconds))
      //             } else {
      //               val maxTime = startExpTime.value.min(wellHalfFilledSeconds)
      //               calculateSignalToNoise(r.groups, signalToNoiseAt)
      //                 .flatMap {
      //                   // degenerate case where the ITC cannot compute a S/N
      //                   case SNCalcResult.SNCalcSuccess(snr) if snr.toBigDecimal <= 0.0 =>
      //                     MonadThrow[F].raiseError(
      //                       CalculationError("No signal can be achieved")
      //                     )
      //                   case SNCalcResult.SNCalcSuccess(snr)                            =>
      //                     itcStep(
      //                       numberOfExposures,
      //                       0,
      //                       startExpTime,
      //                       BigDecimal(0).withUnit[Second],
      //                       snr,
      //                       maxTime.withUnit[Second],
      //                       r,
      //                       0.refined
      //                     )
      //                   case SNCalcResult.WavelengthAtAboveRange(w)                     =>
      //                     MonadThrow[F].raiseError(
      //                       CalculationError(
      //                         f"S/N at ${Wavelength.decimalNanometers.reverseGet(w)}%.0f nm above range"
      //                       )
      //                     )
      //                   case SNCalcResult.WavelengthAtBelowRange(w)                     =>
      //                     MonadThrow[F].raiseError(
      //                       CalculationError(
      //                         f"S/N at ${Wavelength.decimalNanometers.reverseGet(w)}%.0f nm below range"
      //                       )
      //                     )
      //                   case r                                                          =>
      //                     MonadThrow[F].raiseError(CalculationError(r.toString))
      //                 }

      //             }
      //           }

      //         }
      //         .map(NonEmptyChain.one)
      //     }

      // }

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes.
       */
      private def spectroscopySNAt(
        target:          TargetData,
        band:            Band,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        signalToNoise:   SignalToNoise,
        signalToNoiseAt: Wavelength
      ): F[NonEmptyChain[IntegrationTime]] =
        for {
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target at band $band")
          r <-
            T.span("itc.calctime.spectroscopy-exp-time-at") {
              itcWithSNAt(
                target,
                band,
                observingMode,
                constraints,
                signalToNoise,
                signalToNoiseAt
              )
            }
          t <- calculationResults(r)
        } yield t

      private def imagingLegacy(
        target:        TargetData,
        band:          Band,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        sigma:         SignalToNoise
      ): F[legacy.ExposureTimeRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query") {
          val request =
            imagingParams(target, band, observingMode, constraints, sigma).asJson

          for {
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.sigma" -> sigma.toBigDecimal.toDouble)
            _ <- L.info(request.noSpaces)
            r <- itcLocal.calculateExposureTime(request.noSpaces)
          } yield r
        }

      private def calculationResults(
        r: ExposureTimeRemoteResult
      ): F[NonEmptyChain[IntegrationTime]] =
        r.exposureCalculation.traverse(r =>
          TimeSpan
            .fromSeconds(r.exposureTime)
            .map(expTime => IntegrationTime(expTime, r.exposureCount, r.signalToNoise).pure[F])
            .getOrElse {
              MonadThrow[F].raiseError(
                CalculationError(
                  s"Negative exposure time ${r.exposureTime}"
                )
              )
            }
        )

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      private def imaging(
        target:        TargetData,
        band:          Band,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[NonEmptyChain[IntegrationTime]] =
        for {
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target  at band $band")
          r <- T.span("itc.calctime.spectroscopy-exp-time-at") {
                 imagingLegacy(target, band, observingMode, constraints, signalToNoise)
               }
          t <- calculationResults(r)
        } yield t

    }
}
