// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import algebra.instances.all.given
import cats.ApplicativeError
import cats.ApplicativeThrow
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import coulomb.units.si.given
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.itc.Itc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.refined.*
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.syntax.all.*
import org.typelevel.log4cats.Logger

import java.time.Duration
import scala.concurrent.duration.*
import scala.math.*

trait SignalToNoiseCalculation[F[_]: cats.Applicative] { this: Itc[F] =>
  def calculateSignalToNoise(
    graph:           NonEmptyList[ItcChartGroup],
    signalToNoiseAt: Option[Wavelength]
  ): F[Itc.SNCalcResult] =
    (for {
      s2nChart     <- graph.flatMap(_.charts).find(_.chartType === ChartType.S2NChart)
      finalS2NData <- s2nChart.series
                        .filter(_.seriesType === SeriesDataType.FinalS2NData)
                        .map(_.data)
                        .flatten
                        .some
    } yield {
      val sorted               = finalS2NData.sortBy(_._1)
      val sn: Itc.SNCalcResult = signalToNoiseAt
        .fold(Itc.SNCalcResult.SNCalcSuccess(sorted.maxBy(_._2)._2)) { w =>
          val nanos = w.nanometer.value.toDouble
          if (nanos < sorted.head._1) Itc.SNCalcResult.WavelengthAtBelowRange(w)
          else if (nanos > sorted.last._1) Itc.SNCalcResult.WavelengthAtAboveRange(w)
          else
            val sortedList = sorted.toList
            val index      = sortedList.indexWhere(_._1 >= nanos)
            sortedList.lift(index).fold(Itc.SNCalcResult.NoData()) { secondPoint =>
              val (w2, s2) = secondPoint
              if (w2 === nanos) {
                Itc.SNCalcResult.SNCalcSuccess(s2)
              } else {
                sortedList.lift(index - 1) match
                  case Some((w1, s1)) =>
                    // Linear interpolation
                    val sn = (s1 * (w2 - nanos) + s2 * (nanos - w1)) / (w2 - w1)
                    Itc.SNCalcResult.SNCalcSuccess(sn)
                  case _              =>
                    // We are checking the limits before, this shouldn't happen
                    Itc.SNCalcResult.NoData()
              }
            }
        }
      sn.pure[F]
    }).getOrElse(Itc.SNCalcResult.NoData().pure[F])

}

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type NumberOfExposures = Int

  def forHeroku[F[_]: Async: Logger: Trace]: Resource[F, Itc[F]] =
    forUri(uri"https://gemini-new-itc.herokuapp.com")

  val Error400Regex = "<title>Error 400 (.*)</title>".r

  def forUri[F[_]: Async: Logger: Trace](uri: Uri): Resource[F, Itc[F]] =
    EmberClientBuilder.default.build
      .map(NatchezMiddleware.client[F])
      .map(RequestLogger(true, false))
      .map(ResponseLogger(true, false))
      .map(forClientAndUri[F](_, uri))

  def forClientAndUri[F[_]: Concurrent: Logger: Trace](c: Client[F], uri: Uri): Itc[F] =
    new Itc[F] with Http4sClientDsl[F] with SignalToNoiseCalculation[F] {
      val L = Logger[F]

      def calculateExposureTime(
        targetProfile:   TargetProfile,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        signalToNoise:   BigDecimal,
        signalToNoiseAt: Option[Wavelength]
      ): F[Itc.CalcResultWithVersion] =
        observingMode match
          case _: ObservingMode.Spectroscopy =>
            spectroscopy(targetProfile, observingMode, constraints, signalToNoise, signalToNoiseAt)
          // TODO: imaging

      def calculateGraph(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  NonNegDuration,
        exposures:     PosLong
      ): F[Itc.GraphResult] =
        observingMode match
          case _: ObservingMode.Spectroscopy =>
            spectroscopyGraph(
              targetProfile,
              observingMode,
              constraints,
              BigDecimal(exposureTime.value.toMillis).withUnit[Millisecond].toUnit[Second],
              exposures.value
            )
          // TODO: imaging

      // Convenience method to compute an OCS2 ITC result for the specified profile/mode.
      def itc(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Int,
        level:            NonNegInt
      ): F[legacy.ItcRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        Trace[F].span("legacy-itc-query") {
          val json =
            spectroscopyParams(targetProfile,
                               observingMode,
                               exposureDuration.value.toDouble.seconds,
                               constraints,
                               exposures
            ).asJson
          L.info(s"ITC remote query ${uri / "jsonchart"} ${json.noSpaces}") *>
            Trace[F].put("itc.query" -> json.spaces2) *>
            Trace[F].put("itc.exposureDuration" -> exposureDuration.value.toInt) *>
            Trace[F].put("itc.exposures" -> exposures) *>
            Trace[F].put("itc.level" -> level.value) *>
            c.run(POST(json, uri / "jsonchart")).use {
              case Status.Successful(resp) =>
                given EntityDecoder[F, ItcRemoteResult] = jsonOf[F, ItcRemoteResult]
                resp.as[ItcRemoteResult]
              case resp                    =>
                resp.bodyText
                  .through(fs2.text.lines)
                  .filter(_.startsWith("<title>"))
                  .compile
                  .last
                  .flatMap {
                    case Some(Error400Regex(msg)) =>
                      L.warn(s"Upstream error $msg") *>
                        ApplicativeError[F, Throwable].raiseError(new UpstreamException(msg))
                    case u                        =>
                      L.warn(s"Upstream error ${u}") *>
                        ApplicativeError[F, Throwable]
                          .raiseError(new UpstreamException(u.getOrElse("Upstream Exception")))
                  }
            }
        }

      override def itcVersions: F[String] =
        import lucuma.itc.legacy.*

        L.info(s"ITC remote query for versions") *>
          c.run(GET(uri / "version")).use {
            case Status.Successful(resp) =>
              given EntityDecoder[F, ItcRemoteVersion] = jsonOf[F, ItcRemoteVersion]
              resp.as[ItcRemoteVersion].map(_.versionToken)
            case resp                    =>
              L.warn(s"Upstream error ${resp}") *>
                ApplicativeError[F, Throwable]
                  .raiseError(new UpstreamException("Upstream Exception"))
          }

      def itcGraph(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Long
      ): F[legacy.ItcRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        Trace[F].span("legacy-itc-query") {
          val json =
            spectroscopyParams(targetProfile,
                               observingMode,
                               exposureDuration.value.toDouble.seconds,
                               constraints,
                               exposures.toInt
            ).asJson
          L.info(s"ITC remote query ${uri / "jsonchart"} ${json.noSpaces}") *>
            Trace[F].put("itc.query" -> json.spaces2) *>
            Trace[F].put("itc.exposureDuration" -> exposureDuration.value.toInt) *>
            Trace[F].put("itc.exposures" -> exposures.toInt) *>
            c.run(POST(json, uri / "jsonchart")).use {
              case Status.Successful(resp) =>
                given EntityDecoder[F, ItcRemoteResult] = jsonOf[F, ItcRemoteResult]
                resp.as[ItcRemoteResult]
              case resp                    =>
                resp.bodyText
                  .through(fs2.text.lines)
                  .filter(_.startsWith("<title>"))
                  .compile
                  .last
                  .flatMap {
                    case Some(Error400Regex(msg)) =>
                      L.warn(s"Upstream error $msg") *>
                        ApplicativeError[F, Throwable].raiseError(new UpstreamException(msg))
                    case u                        =>
                      L.warn(s"Upstream error ${u}") *>
                        ApplicativeError[F, Throwable]
                          .raiseError(new UpstreamException(u.getOrElse("Upstream Exception")))
                  }
            }
        }

      val MaxIterations = 10

      def spectroscopyGraph(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Long
      ): F[Itc.GraphResult] =
        itcGraph(targetProfile, observingMode, constraints, exposureDuration, exposures).map { r =>
          Itc.GraphResult.fromLegacy(r.versionToken, r.ccds, r.groups)
        }

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      def spectroscopy(
        targetProfile:   TargetProfile,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        signalToNoise:   BigDecimal,
        signalToNoiseAt: Option[Wavelength]
      ): F[Itc.CalcResultWithVersion] = {
        val startExpTime      = BigDecimal(1200.0).withUnit[Second]
        val numberOfExposures = 1
        val requestedSN       = signalToNoise.toDouble

        // This loops should be necessary only a few times but put a circuit breaker just in case
        def itcStep(
          nExp:       NumberOfExposures,
          oldNExp:    Int,
          expTime:    Quantity[BigDecimal, Second],
          oldExpTime: Quantity[BigDecimal, Second],
          snr:        Double,
          maxTime:    Quantity[BigDecimal, Second],
          s:          legacy.ItcRemoteResult,
          counter:    NonNegInt
        ): F[Itc.CalcResult] =
          if (snr === 0.0) {
            Concurrent[F].raiseError(new ItcCalculationError("S/N obtained is 0"))
          } else {
            val totalTime: Quantity[BigDecimal, Second] =
              expTime * nExp.withUnit[1] * pow(requestedSN / snr, 2).withUnit[1]

            val newNExp: BigDecimal = spire.math.ceil((totalTime / maxTime).value)

            val newExpTime: BigDecimal =
              spire.math.ceil((totalTime / newNExp.withUnit[1]).value)

            val next = NonNegInt.from(counter.value + 1).getOrElse(sys.error("Should not happen"))
            L.info(s"Total time: $totalTime maxTime: $maxTime") *>
              L.info(s"Exp time :$newExpTime s/Num exp $newNExp/iteration $counter") *> {
                if (
                  nExp != oldNExp ||
                  ((expTime - oldExpTime) > 1.withUnit[Second] || (oldExpTime - expTime) > 1
                    .withUnit[Second]) &&
                  counter.value < MaxIterations &&
                  newExpTime < (pow(2, 63) - 1)
                ) {
                  itc(targetProfile,
                      observingMode,
                      constraints,
                      newExpTime.withUnit[Second],
                      newNExp.toInt,
                      next
                  )
                    .flatMap { s =>
                      L.debug(s"-> S/N: ${s.maxTotalSNRatio}") *>
                        calculateSignalToNoise(s.groups, signalToNoiseAt).flatMap {
                          case Itc.SNCalcResult.SNCalcSuccess(snr) =>
                            itcStep(newNExp.toInt,
                                    nExp,
                                    newExpTime.withUnit[Second],
                                    expTime,
                                    snr.toDouble,
                                    maxTime,
                                    s,
                                    next
                            )
                          case r                                   =>
                            Itc.CalcResult.CalculationError(r.toString).pure[F]
                        }
                    }
                } else
                  Itc.CalcResult
                    .Success(newExpTime.toDouble.seconds, newNExp.toInt, s.maxTotalSNRatio)
                    .pure[F]
                    .widen[Itc.CalcResult]
              }
          }

        L.info(s"Desired S/N $signalToNoise") *>
          L.info(
            s"Target brightness ${targetProfile} at band ${targetProfile.band}"
          ) *>
          Trace[F].span("itc.calctime.spectroscopy") {

            itc(targetProfile,
                observingMode,
                constraints,
                startExpTime,
                numberOfExposures,
                1.refined
            )
              .flatMap { r =>
                val halfWellTime = r.maxWellDepth / 2 / r.maxPeakPixelFlux * startExpTime.value
                L.info(
                  s"Results CCD wellDepth: ${r.maxWellDepth}, peakPixelFlux: ${r.maxPeakPixelFlux}, totalSNRatio: ${r.maxTotalSNRatio} $halfWellTime"
                ) *> {
                  if (halfWellTime < 1.0) {
                    val msg = s"Target is too bright. Well half filled in $halfWellTime"
                    L.error(msg) *>
                      Itc.CalcResultWithVersion(Itc.CalcResult.SourceTooBright(msg)).pure[F].widen
                  } else {
                    val maxTime = startExpTime.value.min(halfWellTime)
                    calculateSignalToNoise(r.groups, signalToNoiseAt)
                      .flatMap {
                        case Itc.SNCalcResult.SNCalcSuccess(snr)        =>
                          itcStep(numberOfExposures,
                                  0,
                                  startExpTime,
                                  BigDecimal(0).withUnit[Second],
                                  snr.toDouble,
                                  maxTime.withUnit[Second],
                                  r,
                                  0.refined
                          )
                        case Itc.SNCalcResult.WavelengthAtAboveRange(w) =>
                          Itc.CalcResult
                            .CalculationError(
                              f"S/N at ${w.nanometer.value.toDouble}%.0f nm above range"
                            )
                            .pure[F]
                        case Itc.SNCalcResult.WavelengthAtBelowRange(w) =>
                          Itc.CalcResult
                            .CalculationError(
                              f"S/N at ${w.nanometer.value.toDouble}%.0f nm below range"
                            )
                            .pure[F]
                        case r                                          =>
                          Itc.CalcResult.CalculationError(r.toString).pure[F]
                      }
                      .map(x => Itc.CalcResultWithVersion(x, r.versionToken.some))
                  }
                }

              }
          }

      }
    }

}
