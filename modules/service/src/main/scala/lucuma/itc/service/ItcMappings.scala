// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats._
import cats.data.*
import cats.derived.*
import cats.effect.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.StringCommands
import edu.gemini.grackle.*
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcVersions
import lucuma.itc.SpectroscopyGraphResult
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.encoders.given
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.hashes.given
import lucuma.itc.service.config.*
import lucuma.itc.service.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import java.time.Duration
import scala.io.Source
import scala.util.Using

import Query.*
import Value.*
import QueryCompiler.*
import lucuma.odb.graphql.input.WavelengthInput
import lucuma.odb.graphql.input.sourceprofile.SourceProfileInput
import lucuma.odb.graphql.input.sourceprofile.BandNormalizedInput
import lucuma.odb.graphql.input.sourceprofile.SpectralDefinitionInput
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam

case class GraphRequest(
  targetProfile:      TargetProfile,
  specMode:           ObservingMode.SpectroscopyMode,
  constraints:        ItcObservingConditions,
  expTime:            NonNegDuration,
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
  specMode:      ObservingMode.ImagingMode,
  constraints:   ItcObservingConditions,
  signalToNoise: SignalToNoise
) derives Hash

object ItcMapping extends ItcCacheOrRemote with Version with GracklePartials {

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

  //   (env.get[Wavelength]("wavelength"),
  //    env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
  //    env.get[SignalToNoise]("signalToNoise"),
  //    env.get[SourceProfile]("sourceProfile"),
  //    env.get[ImagingParams]("mode"),
  //    env.get[Band]("band"),
  //    env.get[ItcObservingConditions]("constraints")
  //   ).traverseN { (wv, rs, sn, sp, mode, band, c) =>
  //     val imgMode = mode match {
  //       case GmosNImagingParams(filter) =>
  //         ObservingMode.ImagingMode.GmosNorth(wv, filter)
  //       case GmosSImagingParams(filter) =>
  //         ObservingMode.ImagingMode.GmosSouth(wv, filter)
  //     }
  //     imgTimeFromCacheOrRemote(
  //       ImagingIntegrationTimeRequest(
  //         TargetProfile(sp, band, rs),
  //         imgMode,
  //         c,
  //         sn
  //       )
  //     )(itc, redis).map { r =>
  //       Result(
  //         IntegrationTimeCalculationResult(
  //           version(environment).value,
  //           BuildInfo.ocslibHash,
  //           imgMode,
  //           r
  //         )
  //       )
  //     }
  //   }
  //
  private def toSpectroscopyTimeRequest(
    input: SpectroscopyIntegrationTimeInput
  ): Result[SpectroscopyIntegrationTimeRequest] = {
    val SpectroscopyIntegrationTimeInput(wavelength,
                                         signalToNoiseAt,
                                         signalToNoise,
                                         sourceProfile,
                                         band,
                                         radialVelocity,
                                         constraints,
                                         mode
    ) = input

    val redshift = Result.fromOption(radialVelocity.toRedshift, "Invalid radial velocity")
    val specMode = mode match {
      case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter)
        )
      case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter)
        )
      case _                                                                 =>
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

  def calculateSpectroscopyIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: SpectroscopyIntegrationTimeRequest): F[IntegrationTimeCalculationResult] =
    specTimeFromCacheOrRemote(input)(itc, redis)
      .adaptError {
        case x: IntegrationTimeError =>
          new RuntimeException(x.message)
        case UpstreamException(msg)  =>
          new RuntimeException(msg.mkString("\n"))
        case x                       =>
          new RuntimeException(s"Error calculating itc $x")
      }
      .map { r =>
        IntegrationTimeCalculationResult(
          version(environment).value,
          BuildInfo.ocslibHash,
          input.specMode,
          r
        )
      }

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
      case GmosNImagingInput(filter) =>
        Result(ObservingMode.ImagingMode.GmosNorth(wavelength, filter))
      case GmosSImagingInput(filter) =>
        Result(ObservingMode.ImagingMode.GmosSouth(wavelength, filter))
      case _                         =>
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
  )(input: ImagingIntegrationTimeRequest): F[IntegrationTimeCalculationResult] =
    imgTimeFromCacheOrRemote(input)(itc, redis)
      .adaptError {
        case x: IntegrationTimeError =>
          new RuntimeException(x.message)
        case UpstreamException(msg)  =>
          new RuntimeException(msg.mkString("\n"))
        case x                       =>
          new RuntimeException(s"Error calculating itc $x")
      }
      .map { r =>
        IntegrationTimeCalculationResult(
          version(environment).value,
          BuildInfo.ocslibHash,
          input.specMode,
          r
        )
      }

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
      case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter)
        )
      case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter)
        )
      case _                                                                 =>
        Result.failure("Invalid spectroscopy mode")
    }

    val itcConditions =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    val expTime = Result.fromEither(NonNegDuration.from(exposureTime.toDuration))

    (redshift, specMode, itcConditions, expTime).parMapN { (rs, mode, conditions, expTime) =>
      GraphRequest(
        TargetProfile(sourceProfile, band, rs),
        mode,
        conditions,
        expTime,
        exposures,
        signalToNoiseAt,
        figures
      )
    }
  }

  // def spectroscopyIntegrationTimeAndGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
  //   environment: ExecutionEnvironment,
  //   redis:       StringCommands[F, Array[Byte], Array[Byte]],
  //   itc:         Itc[F]
  // )(env: Cursor.Env): F[Result[SpectroscopyTimeAndGraphResult]] =
  //   (for {
  //     time         <- IorT(calculateSpectroscopyIntegrationTime(environment, redis, itc)(env))
  //     expTime       = time.results.head.exposureTime
  //     exposures     = time.results.head.exposures
  //     exposureTime <-
  //       IorT
  //         .fromEither[F](
  //           NonNegDuration
  //             .from(expTime.toDuration)
  //         )
  //         .leftMap(u => NonEmptyChain(Problem(u))): IorT[F, NonEmptyChain[Problem], NonNegDuration]
  //     graphEnv      =
  //       env.add("exposureTime" -> exposureTime).add("exposures" -> exposures)
  //     graph        <- IorT(spectroscopyGraph(environment, redis, itc)(graphEnv))
  //   } yield SpectroscopyTimeAndGraphResult.fromTimeAndGraph(expTime, exposures, graph)).value
  //
  def spectroscopyGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: GraphRequest): F[SpectroscopyGraphResult] =
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
        SpectroscopyGraphResult(version(environment).value,
                                BuildInfo.ocslibHash,
                                ccds,
                                charts.flatMap(_.charts),
                                peakFinalSNRatio,
                                atWvFinalSNRatio,
                                peakSingleSNRatio,
                                atWvSingleSNRatio
        )
      }
      .adaptError {
        case x: IntegrationTimeError =>
          new RuntimeException(x.message)
        case UpstreamException(msg)  =>
          new RuntimeException(msg.mkString("\n"))
        case x                       =>
          new RuntimeException(s"Error calculating itc $x")
      }

  //
  // def calculateSignalToNoise[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
  //   environment: ExecutionEnvironment,
  //   redis:       StringCommands[F, Array[Byte], Array[Byte]],
  //   itc:         Itc[F]
  // )(env: Cursor.Env): F[Result[SNCalcResult]] =
  //   (env.get[Wavelength]("wavelength"),
  //    env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
  //    env.get[NonNegDuration]("exposureTime"),
  //    env.get[PosInt]("exposures"),
  //    env.get[SourceProfile]("sourceProfile"),
  //    env.get[Band]("band"),
  //    env.get[SpectroscopyParams]("mode"),
  //    env.get[ItcObservingConditions]("constraints")
  //   ).traverseN { (wv, rs, expTime, exp, sp, sd, mode, c) =>
  //     Logger[F].info(
  //       s"ITC sn calculation for $mode, conditions $c, exposureTime $expTime x $exp and profile $sp"
  //     ) *> {
  //       val signalToNoiseAt = env.get[Wavelength]("signalToNoiseAt")
  //       val specMode        = mode match {
  //         case GmosNSpectroscopyParams(grating, fpu, filter) =>
  //           ObservingMode.SpectroscopyMode.GmosNorth(wv, grating, fpu, filter)
  //         case GmosSSpectroscopyParams(grating, fpu, filter) =>
  //           ObservingMode.SpectroscopyMode.GmosSouth(wv, grating, fpu, filter)
  //       }
  //
  //       graphFromCacheOrRemote(
  //         GraphRequest(TargetProfile(sp, sd, rs), specMode, c, expTime, exp, signalToNoiseAt)
  //       )(itc, redis)
  //         .flatMap { r =>
  //           itc.calculateSignalToNoise(r.charts, signalToNoiseAt)
  //         }
  //     }
  //       .map(_.rightIor[NonEmptyChain[Problem]])
  //       .handleError {
  //         case x: IntegrationTimeError =>
  //           Problem(x.message).leftIorNec
  //         case x                       =>
  //           Problem(s"Error calculating itc $x").leftIorNec
  //       }
  //   }.map(
  //     _.getOrElse(Problem(s"Missing parameters for signal to noise calculation$env").leftIorNec)
  //   )
  //
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
                RootEffect.computeEncodable("versions")((_, p, env) =>
                  versions(environment, redis)
                ),
                RootEffect.computeEncodable("test")((_, p, env) => Result("unsupported").pure[F]),
                RootEffect.computeEncodable("spectroscopyIntegrationTime") { (_, p, env) =>
                  env
                    .getR[SpectroscopyIntegrationTimeInput]("input")
                    .flatMap(toSpectroscopyTimeRequest)
                    .traverse(
                      calculateSpectroscopyIntegrationTime(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("imagingIntegrationTime") { (_, p, env) =>
                  env
                    .getR[ImagingIntegrationTimeInput]("input")
                    .flatMap(toImagingTimeRequest)
                    .traverse(
                      calculateImagingIntegrationTime(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("optimizedSpectroscopyGraph") { (_, p, env) =>
                  env
                    .getR[OptimizedSpectroscopyGraphInput]("input")
                    .flatMap(toGraphRequest)
                    .traverse(
                      spectroscopyGraph(environment, redis, itc)
                    )
                }
                // RootEffect.computeEncodable("spectroscopySignalToNoise")((_, p, env) =>
                //   calculateSignalToNoise(environment, redis, itc)(env)
                // ),
                // RootEffect.computeEncodable("optimizedSpectroscopyGraph")((_, p, env) =>
                //   spectroscopyGraph(environment, redis, itc)(env)
                // ),
                // RootEffect.computeEncodable("imagingIntegrationTime")((_, p, env) =>
                //   calculateImagingIntegrationTime(environment, redis, itc)(env)
                // ),
                // RootEffect.computeEncodable("spectroscopyIntegrationTimeAndGraph")((_, p, env) =>
                //   spectroscopyIntegrationTimeAndGraph(environment, redis, itc)(env)
                // )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[PosInt](PosIntType),
            LeafMapping[SignalToNoise](SignalToNoiseType)
          )

        override val selectElaborator =
          new SelectElaborator(
            Map(
              QueryType -> {
                case Select(
                      "spectroscopyIntegrationTime",
                      List(SpectroscopyIntegrationTimeInput.binding("input", input)),
                      child
                    ) =>
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("spectroscopyIntegrationTime", Nil, child))
                  )

                case Select(
                      "imagingIntegrationTime",
                      List(ImagingIntegrationTimeInput.binding("input", input)),
                      child
                    ) =>
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("imagingIntegrationTime", Nil, child))
                  )

                case Select(
                      "optimizedSpectroscopyGraph",
                      List(OptimizedSpectroscopyGraphInput.binding("input", input)),
                      child
                    ) =>
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("optimizedSpectroscopyGraph", Nil, child))
                  )

                case Select(
                      "test",
                      List(SourceProfileInput.CreateBinding("input", input)),
                      child
                    ) =>
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("test", Nil, child))
                  )
                // input.map { i =>
                //   Select("spectroscopyIntegrationTime", Nil, child)
                //   // val wv = i.wavelength
                // }

//                 case Select("imagingIntegrationTime",
//                             List(Binding("input", ObjectValue(wv))),
//                             child
//                     ) =>
//                   wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
//                     case (e, c) =>
//                       wavelengthPartial
//                         .orElse(radialVelocityPartial)
//                         .orElse(signalToNoisePartial)
//                         .orElse(sourceProfilePartial)
//                         .orElse(bandPartial)
//                         .orElse(instrumentModePartial)
//                         .orElse(constraintsPartial)
//                         .applyOrElse(
//                           (e, c),
//                           fallback
//                         )
//                   }.map(e => e.copy(child = Select("imagingIntegrationTime", Nil, child)))
//
//                 case Select("optimizedSpectroscopyGraph",
//                             List(Binding("input", ObjectValue(wv))),
//                             child
//                     ) =>
//                   wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
//                     case (e, c) =>
//                       wavelengthPartial
//                         .orElse(radialVelocityPartial)
//                         .orElse(exposureTimePartial)
//                         .orElse(exposuresPartial)
//                         .orElse(sourceProfilePartial)
//                         .orElse(bandPartial)
//                         .orElse(instrumentModePartial)
//                         .orElse(constraintsPartial)
//                         .orElse(significantFiguresPartial)
//                         .orElse(signalToNoiseAtPartial)
//                         .applyOrElse(
//                           (e, c),
//                           fallback
//                         )
//                   }.map(e => e.copy(child = Select("optimizedSpectroscopyGraph", Nil, child)))
//
//                 case Select("spectroscopyIntegrationTimeAndGraph",
//                             List(Binding("input", ObjectValue(wv))),
//                             child
//                     ) =>
//                   wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
//                     case (e, c) =>
//                       wavelengthPartial
//                         .orElse(signalToNoisePartial)
//                         .orElse(signalToNoiseAtPartial)
//                         .orElse(sourceProfilePartial)
//                         .orElse(bandPartial)
//                         .orElse(radialVelocityPartial)
//                         .orElse(constraintsPartial)
//                         .orElse(instrumentModePartial)
//                         .orElse(significantFiguresPartial)
//                         .applyOrElse(
//                           (e, c),
//                           fallback
//                         )
//                   }.map(e =>
//                     e.copy(child = Select("spectroscopyIntegrationTimeAndGraph", Nil, child))
//                   )
//                 case Select("spectroscopySignalToNoise",
//                             List(Binding("input", ObjectValue(wv))),
//                             child
//                     ) =>
//                   wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
//                     case (e, c) =>
//                       wavelengthPartial
//                         .orElse(radialVelocityPartial)
//                         .orElse(exposureTimePartial)
//                         .orElse(exposuresPartial)
//                         .orElse(sourceProfilePartial)
//                         .orElse(bandPartial)
//                         .orElse(instrumentModePartial)
//                         .orElse(constraintsPartial)
//                         .orElse(significantFiguresPartial)
//                         .orElse(signalToNoiseAtPartial)
//                         .applyOrElse(
//                           (e, c),
//                           fallback
//                         )
//                   }.map(e => e.copy(child = Select("spectroscopySignalToNoise", Nil, child)))
              }
            )
          )
      }
    }
}
