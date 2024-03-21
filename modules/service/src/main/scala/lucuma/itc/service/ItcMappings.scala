// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats.*
import cats.derived.*
import cats.effect.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.StringCommands
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import grackle.*
import grackle.QueryCompiler.Elab
import grackle.circe.CirceMapping
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.{ExecutionEnvironment as _, *}
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.ItcVersions
import lucuma.itc.SpectroscopyGraphResult
import lucuma.itc.encoders.given
import lucuma.itc.input.*
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.hashes.given
import lucuma.itc.service.config.*
import lucuma.itc.service.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.io.Source
import scala.util.Using

import Query.*
import QueryCompiler.*

case class GraphRequest(
  targetProfile:      TargetProfile,
  specMode:           ObservingMode.SpectroscopyMode,
  constraints:        ItcObservingConditions,
  expTime:            TimeSpan,
  exp:                PosInt,
  signalToNoiseAt:    Option[Wavelength],
  significantFigures: Option[SignificantFigures]
) derives Hash

case class SpectroscopyIntegrationTimeRequest(
  targetProfile:   TargetProfile,
  specMode:        ObservingMode.SpectroscopyMode,
  constraints:     ItcObservingConditions,
  signalToNoise:   SignalToNoise,
  signalToNoiseAt: Option[Wavelength]
) derives Hash

case class ImagingIntegrationTimeRequest(
  targetProfile: TargetProfile,
  imagingMode:   ObservingMode.ImagingMode,
  constraints:   ItcObservingConditions,
  signalToNoise: SignalToNoise
) derives Hash

object ItcMapping extends ItcCacheOrRemote with Version {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync: Logger]: F[Schema] =
    Sync[F]
      .defer {
        Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
          Schema(src.mkString).toEither.fold(
            x => sys.error(s"Invalid schema: ${x.toList.mkString(", ")}"),
            identity
          )
        }.liftTo[F]
      }

  private def toSpectroscopyTimeRequest(
    input: SpectroscopyTimeInput
  ): Result[SpectroscopyIntegrationTimeRequest] = {
    val wavelength      = input.wavelength
    val signalToNoiseAt = input.signalToNoiseAt
    val signalToNoise   = input.signalToNoise
    val sourceProfile   = input.sourceProfile
    val band            = input.band
    val radialVelocity  = input.radialVelocity
    val constraints     = input.constraints
    val mode            = input.mode

    val redshift = Result.fromOption(radialVelocity.toRedshift, "Invalid radial velocity")
    val specMode = mode match {
      case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter, ccdMode, roi)
        )
      case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter, ccdMode, roi)
        )
      case _                                                                               =>
        Result.failure("Invalid spectroscopy mode")
    }

    val itcConditions =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (redshift, specMode, itcConditions).parMapN { (rs, mode, conditions) =>
      SpectroscopyIntegrationTimeRequest(
        TargetProfile(sourceProfile, band, rs),
        mode,
        conditions,
        signalToNoise,
        signalToNoiseAt
      )
    }
  }

  private def recoverResult[A]: PartialFunction[Throwable, Result[A]] = {
    case x @ SourceTooBright(hw) =>
      Result.failure(
        Problem(x.message, extensions = SourceTooBrightExtension(hw).asJsonObject.some)
      )
    case x: IntegrationTimeError =>
      Result.failure(x.message)
    case UpstreamException(msg)  =>
      Result.failure(msg.mkString("\n"))
    case x                       =>
      Result.failure(s"Error calculating itc $x")
  }

  def calculateSpectroscopyIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: SpectroscopyIntegrationTimeRequest): F[Result[IntegrationTimeCalculationResult]] =
    specTimeFromCacheOrRemote(input)(itc, redis)
      .map { r =>
        Result(
          IntegrationTimeCalculationResult(
            version(environment).value,
            BuildInfo.ocslibHash,
            input.specMode,
            Zipper.fromNel(r) // In spectroscopy we always get a single result
          )
        )
      }
      .recover(recoverResult)

  private def toImagingTimeRequest(
    input: ImagingIntegrationTimeInput
  ): Result[ImagingIntegrationTimeRequest] = {
    val ImagingIntegrationTimeInput(wavelength,
                                    signalToNoise,
                                    sourceProfile,
                                    band,
                                    radialVelocity,
                                    constraints,
                                    mode
    ) = input

    val redshift = Result.fromOption(radialVelocity.toRedshift, "Invalid radial velocity")
    val specMode = mode match {
      case GmosNImagingInput(filter, ccdMode) =>
        Result(
          ObservingMode.ImagingMode.GmosNorth(wavelength, filter, ccdMode)
        )
      case GmosSImagingInput(filter, ccdMode) =>
        Result(ObservingMode.ImagingMode.GmosSouth(wavelength, filter, ccdMode))
      case _                                  =>
        Result.failure("Invalid spectroscopy mode")
    }

    val itcConditions =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))
    (redshift, specMode, itcConditions).parMapN { (rs, mode, conditions) =>
      ImagingIntegrationTimeRequest(
        TargetProfile(sourceProfile, band, rs),
        mode,
        conditions,
        signalToNoise
      )
    }
  }

  def calculateImagingIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: ImagingIntegrationTimeRequest): F[Result[IntegrationTimeCalculationResult]] =
    imgTimeFromCacheOrRemote(input)(itc, redis)
      .map { r =>
        Result(
          IntegrationTimeCalculationResult(
            version(environment).value,
            BuildInfo.ocslibHash,
            input.imagingMode,
            // This is mode specific
            input.imagingMode match {
              case ObservingMode.ImagingMode.GmosNorth(_, _, _) |
                  ObservingMode.ImagingMode.GmosSouth(_, _, _) =>
                Zipper
                  .fromNel(r)
                  .focusIndex(1)
                  .getOrElse(Zipper.fromNel(r)) // For gmos focus on the second CCD
            }
          )
        )
      }
      .recover(recoverResult)

  private def toGraphRequest(
    input: OptimizedSpectroscopyGraphInput
  ): Result[GraphRequest] = {
    val OptimizedSpectroscopyGraphInput(wavelength,
                                        signalToNoiseAt,
                                        exposureTime,
                                        exposures,
                                        sourceProfile,
                                        band,
                                        radialVelocity,
                                        constraints,
                                        mode,
                                        figures
    ) = input

    val redshift = Result.fromOption(radialVelocity.toRedshift, "Invalid radial velocity")
    val specMode = mode match {
      case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter, ccdMode, roi)
        )
      case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter, ccdMode, roi)
        )
      case _                                                                               =>
        Result.failure("Invalid spectroscopy mode")
    }

    val itcConditions =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (redshift, specMode, itcConditions).parMapN { (rs, mode, conditions) =>
      GraphRequest(
        TargetProfile(sourceProfile, band, rs),
        mode,
        conditions,
        exposureTime,
        exposures,
        signalToNoiseAt,
        figures
      )
    }
  }

  def spectroscopyIntegrationTimeAndGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(
    tr:          SpectroscopyIntegrationTimeRequest,
    fig:         Option[SignificantFigures]
  ): F[Result[SpectroscopyTimeAndGraphResult]] =
    (for {
      specTime <-
        ResultT(calculateSpectroscopyIntegrationTime(environment, redis, itc)(tr))
      expTime   = specTime.results.focus.exposureTime
      exps      = specTime.results.focus.exposures
      req       = GraphRequest(tr.targetProfile,
                               tr.specMode,
                               tr.constraints,
                               expTime,
                               exps,
                               tr.signalToNoiseAt,
                               fig
                  )
      gr       <- ResultT(spectroscopyGraph(environment, redis, itc)(req))
    } yield SpectroscopyTimeAndGraphResult(
      gr.serverVersion,
      gr.dataVersion,
      expTime,
      exps,
      gr.ccds,
      gr.charts,
      gr.peakFinalSNRatio,
      gr.atWavelengthFinalSNRatio,
      gr.peakSingleSNRatio,
      gr.atWavelengthSingleSNRatio
    )).value

  def spectroscopyGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: GraphRequest): F[Result[SpectroscopyGraphResult]] =
    graphFromCacheOrRemote(input)(itc, redis)
      .map { r =>
        val charts                              =
          input.significantFigures.fold(r.charts)(v => r.charts.map(_.adjustSignificantFigures(v)))
        val ccds                                =
          input.significantFigures.fold(r.ccds)(v => r.ccds.map(_.adjustSignificantFigures(v)))
        val peakFinalSNRatio                    =
          input.significantFigures.fold(r.peakFinalSNRatio)(
            r.peakFinalSNRatio.adjustSignificantFigures
          )
        val peakSingleSNRatio                   =
          input.significantFigures.fold(r.peakSingleSNRatio)(
            r.peakSingleSNRatio.adjustSignificantFigures
          )
        val atWvFinalSNRatio: Option[FinalSN]   =
          input.significantFigures.fold(r.atWavelengthFinalSNRatio)(s =>
            r.atWavelengthFinalSNRatio.map(_.adjustSignificantFigures(s))
          )
        val atWvSingleSNRatio: Option[SingleSN] =
          input.significantFigures.fold(r.atWavelengthSingleSNRatio)(s =>
            r.atWavelengthSingleSNRatio.map(_.adjustSignificantFigures(s))
          )
        Result(
          SpectroscopyGraphResult(version(environment).value,
                                  BuildInfo.ocslibHash,
                                  ccds,
                                  charts.flatMap(_.charts),
                                  peakFinalSNRatio,
                                  atWvFinalSNRatio,
                                  peakSingleSNRatio,
                                  atWvSingleSNRatio
          )
        )
      }
      .recover(recoverResult)

  def versions[F[_]: Applicative: Logger](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]]
  ): F[Result[ItcVersions]] =
    Result(ItcVersions(version(environment).value, BuildInfo.ocslibHash.some)).pure[F]

  def apply[F[_]: Sync: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  ): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] {

        override def parserConfig: GraphQLParser.Config =
          GraphQLParser.defaultConfig.copy(maxInputValueDepth =
            16
          ) // set a more reasonable input depth limit

        val schema: Schema    = loadedSchema
        val QueryType         = schema.ref("Query")
        val BigDecimalType    = schema.ref("BigDecimal")
        val LongType          = schema.ref("Long")
        val PosIntType        = schema.ref("PosInt")
        val SignalToNoiseType = schema.ref("SignalToNoise")

        val typeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                RootEffect.computeEncodable("versions")((p, env) => versions(environment, redis)),
                RootEffect.computeEncodable("test")((p, env) => Result("unsupported").pure[F]),
                RootEffect.computeEncodable("spectroscopyIntegrationTime") { (p, env) =>
                  env
                    .getR[SpectroscopyIntegrationTimeInput]("input")
                    .flatMap(toSpectroscopyTimeRequest)
                    .flatTraverse(
                      calculateSpectroscopyIntegrationTime(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("imagingIntegrationTime") { (p, env) =>
                  env
                    .getR[ImagingIntegrationTimeInput]("input")
                    .flatMap(toImagingTimeRequest)
                    .flatTraverse(
                      calculateImagingIntegrationTime(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("optimizedSpectroscopyGraph") { (p, env) =>
                  env
                    .getR[OptimizedSpectroscopyGraphInput]("input")
                    .flatMap(toGraphRequest)
                    .flatTraverse(
                      spectroscopyGraph(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("spectroscopyIntegrationTimeAndGraph") { (p, env) =>
                  env
                    .getR[SpectroscopyIntegrationTimeAndGraphInput]("input")
                    .flatMap(u => toSpectroscopyTimeRequest(u).map((_, u.significantFigures)))
                    .flatTraverse { case (tr, fig) =>
                      spectroscopyIntegrationTimeAndGraph(environment, redis, itc)(tr, fig)
                    }
                }
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[PosInt](PosIntType),
            LeafMapping[SignalToNoise](SignalToNoiseType)
          )

        override val selectElaborator =
          def handle[A](input: Result[A]): Elab[Unit] =
            Elab.liftR(input).flatMap(i => Elab.env("input" -> i))
          SelectElaborator {
            case (QueryType,
                  "spectroscopyIntegrationTime",
                  List(SpectroscopyIntegrationTimeInput.binding("input", input))
                ) =>
              handle(input)
            case (QueryType,
                  "imagingIntegrationTime",
                  List(ImagingIntegrationTimeInput.binding("input", input))
                ) =>
              handle(input)
            case (QueryType,
                  "optimizedSpectroscopyGraph",
                  List(OptimizedSpectroscopyGraphInput.binding("input", input))
                ) =>
              handle(input)
            case (QueryType,
                  "spectroscopyIntegrationTimeAndGraph",
                  List(SpectroscopyIntegrationTimeAndGraphInput.binding("input", input))
                ) =>
              handle(input)
          }

      }
    }
}
