// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import algebra.instances.all.given
import cats.*
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.legacy.FLocalItc
import lucuma.itc.legacy.IntegrationTimeRemoteResult
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.math.*

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type ExposureCount = Int

  def build[F[_]: MonadThrow: Logger: Trace](itcLocal: FLocalItc[F]): Itc[F] =
    new Itc[F] {
      val L = Logger[F]
      val T = Trace[F]

      def calculateIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        T.span("calculate-integration-time"):
          observingMode match
            case s @ (ObservingMode.SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
                ObservingMode.SpectroscopyMode.GmosSouth(_, _, _, _, _, _)) =>
              spectroscopyIntegrationTime(target, atWavelength, s, constraints, signalToNoise)
            case i @ (
                  ObservingMode.ImagingMode.GmosNorth(_, _) |
                  ObservingMode.ImagingMode.GmosSouth(_, _)
                ) =>
              imaging(target, atWavelength, i, constraints, signalToNoise)

      def calculateGraphs(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetGraphsCalcResult] =
        observingMode match
          case s @ (ObservingMode.SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
              ObservingMode.SpectroscopyMode.GmosSouth(_, _, _, _, _, _)) =>
            spectroscopyGraphs(
              target,
              atWavelength,
              s,
              constraints,
              exposureTime.toMilliseconds.withUnit[Millisecond].toUnit[Second],
              exposureCount.value
            )
          case ObservingMode.ImagingMode.GmosNorth(_, _) |
              ObservingMode.ImagingMode.GmosSouth(_, _) =>
            MonadThrow[F].raiseError:
              new IllegalArgumentException("Imaging mode not supported for graph calculation")

      private def spectroscopyGraphs(
        target:           TargetData,
        atWavelength:     Wavelength,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposureCount:    Int,
        level:            Option[NonNegInt] = none
      ): F[TargetGraphsCalcResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query"):
          val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
            spectroscopyGraphParams(
              target,
              atWavelength,
              observingMode,
              exposureDuration.value.toDouble.seconds,
              constraints,
              exposureCount
            ).leftMap(_.asJson)

          for
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.exposureDuration" -> exposureDuration.value.toInt)
            _ <- T.put("itc.exposures" -> exposureCount)
            _ <- T.put("itc.level" -> level.map(_.value).orEmpty)
            r <- itcLocal.calculateGraphs(request.noSpaces)
          yield TargetGraphsCalcResult.fromLegacy(r.ccds, r.groups, atWavelength, bandOrLine)

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes.
       */
      private def spectroscopyIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.SpectroscopyMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          spectroscopyExposureTimeParams(
            target,
            atWavelength,
            observingMode,
            constraints,
            signalToNoise
          ).leftMap(_.asJson)

        for
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target at wavelength $atWavelength")
          r <- T.span("itc.calctime.spectroscopy-integration-time"):
                 for
                   _      <- T.put("itc.query" -> request.spaces2)
                   _      <- T.put("itc.sigma" -> signalToNoise.toBigDecimal.toDouble)
                   _      <- L.info(request.noSpaces) // Request to the legacy itc
                   a      <- itcLocal.calculateIntegrationTime(request.noSpaces)
                   result <- convertIntegrationTimeRemoteResult(a, bandOrLine)
                 yield result
        yield r

      private def convertIntegrationTimeRemoteResult(
        r:          IntegrationTimeRemoteResult,
        bandOrLine: Either[Band, Wavelength]
      ): F[TargetIntegrationTime] =
        r.exposureCalculation
          .traverse: r =>
            TimeSpan
              .fromSeconds(r.exposureTime)
              .map(expTime => IntegrationTime(expTime, r.exposureCount, r.signalToNoise).pure[F])
              .getOrElse:
                MonadThrow[F].raiseError:
                  CalculationError(s"Negative exposure time ${r.exposureTime}")
          .map: ccdTimes =>
            TargetIntegrationTime(Zipper.of(ccdTimes.head, ccdTimes.tail.toList*), bandOrLine)

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      private def imaging(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.ImagingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          imagingParams(target, atWavelength, observingMode, constraints, signalToNoise).leftMap(
            _.asJson
          )

        for
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target  at wavelength $atWavelength")
          r <- T.span("itc.calctime.spectroscopy-exp-time-at"):
                 for
                   _            <- T.put("itc.query" -> request.spaces2)
                   _            <- T.put("itc.sigma" -> signalToNoise.toBigDecimal.toDouble)
                   _            <- L.info(request.noSpaces)
                   remoteResult <- itcLocal.calculateIntegrationTime(request.noSpaces)
                   result       <- convertIntegrationTimeRemoteResult(remoteResult, bandOrLine)
                 yield result
        yield r

    }
}
