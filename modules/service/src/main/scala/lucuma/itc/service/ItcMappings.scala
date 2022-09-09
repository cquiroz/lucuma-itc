// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats._
import cats.data.*
import cats.derived.*
import cats.effect.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.StringCommands
import edu.gemini.grackle.*
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.*
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.itc.*
import lucuma.itc.encoders.given
import lucuma.itc.search.ItcObservationDetails
import lucuma.itc.search.ItcVersions
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.Result.Spectroscopy
import lucuma.itc.search.SpectroscopyGraphResults
import lucuma.itc.search.SpectroscopyResults
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.hashes.given
import lucuma.itc.service.config.*
import lucuma.itc.service.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._
import scala.io.Source
import scala.util.Using

import Query._
import Value._
import QueryCompiler._

case class GraphRequest(
  targetProfile: TargetProfile,
  specMode:      ObservingMode.Spectroscopy,
  constraints:   ItcObservingConditions,
  expTime:       NonNegDuration,
  exp:           PosLong
) derives Hash

case class CalcRequest(
  targetProfile: TargetProfile,
  specMode:      ObservingMode.Spectroscopy,
  constraints:   ItcObservingConditions,
  signalToNoise: PosBigDecimal
) derives Hash

object ItcMapping extends ItcCacheOrRemote with Version with GracklePartials {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F]
      .defer {
        Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
          Schema(src.mkString).right.get
        }.liftTo[F]
      }

  def spectroscopy[F[_]: MonadThrow: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(env:         Cursor.Env): F[Result[List[SpectroscopyResults]]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[PosBigDecimal]("signalToNoise"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[List[SpectroscopyParams]]("modes"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, sn, sp, sd, modes, c) =>
      modes
        .parTraverse { mode =>
          Logger[F].info(s"ITC calculate for $mode, conditions $c and profile $sp") *>
            Trace[F].put(("itc.modes_count", modes.length)) *> {
              val specMode = mode match {
                case GmosNITCParams(grating, fpu, filter) =>
                  ObservingMode.Spectroscopy.GmosNorth(wv, grating, fpu, filter)
                case GmosSITCParams(grating, fpu, filter) =>
                  ObservingMode.Spectroscopy.GmosSouth(wv, grating, fpu, filter)
              }
              calcFromCacheOrRemote(
                CalcRequest(
                  TargetProfile(sp, sd, rs),
                  specMode,
                  c,
                  sn
                )
              )(itc, redis)
                .handleErrorWith {
                  case UpstreamException(msg) =>
                    Itc.CalcResultWithVersion(Itc.CalcResult.CalculationError(msg)).pure[F].widen
                  case x                      =>
                    Itc
                      .CalcResultWithVersion(
                        Itc.CalcResult.CalculationError(s"Error calculating itc $x")
                      )
                      .pure[F]
                      .widen
                }
                .map { r =>
                  SpectroscopyResults(version(environment).value,
                                      r.dataVersion,
                                      List(Spectroscopy(specMode, r.result))
                  )
                }
            }
        }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleErrorWith { case x =>
          Problem(s"Error calculating itc $x").leftIorNec.pure[F]
        }
    }.map(_.getOrElse(Problem("Missing parameters for spectroscopy").leftIorNec))

  def spectroscopyGraph[F[_]: MonadThrow: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(env:         Cursor.Env): F[Result[SpectroscopyGraphResults]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[NonNegDuration]("exposureTime"),
     env.get[PosLong]("exposures"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[SpectroscopyParams]("mode"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, expTime, exp, sp, sd, mode, c) =>
      Logger[F].info(
        s"ITC graph calculate for $mode, conditions $c, exposureTime $expTime x $exp and profile $sp"
      ) *> {
        val significantFigures = env.get[SignificantFigures]("significantFigures")
        val specMode           = mode match {
          case GmosNITCParams(grating, fpu, filter) =>
            ObservingMode.Spectroscopy.GmosNorth(wv, grating, fpu, filter)
          case GmosSITCParams(grating, fpu, filter) =>
            ObservingMode.Spectroscopy.GmosSouth(wv, grating, fpu, filter)
        }
        graphFromCacheOrRemote(
          GraphRequest(TargetProfile(sp, sd, rs), specMode, c, expTime, exp)
        )(itc, redis)
          .map { r =>
            val charts =
              significantFigures.fold(r.charts)(v => r.charts.map(_.adjustSignificantFigures(v)))
            val ccds   =
              significantFigures.fold(r.ccds)(v => r.ccds.map(_.adjustSignificantFigures(v)))
            SpectroscopyGraphResults(version(environment).value,
                                     r.dataVersion.some,
                                     ccds,
                                     charts.flatMap(_.charts)
            )
          }
      }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleError { case x =>
          Problem(s"Error calculating itc $x").leftIorNec
        }
    }.map(_.getOrElse(Problem(s"Missing parameters for spectroscopy graph $env").leftIorNec))

  def versions[F[_]: MonadThrow: Logger](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(
    env:         Cursor.Env
  ): F[Result[ItcVersions]] =
    versionFromCacheOrRemote(environment, redis, itc)
      .map(_.rightIor[NonEmptyChain[Problem]])
      .handleError { case x =>
        Problem(s"Error getting the itc version $x").leftIorNec
      }

  def apply[F[_]: Sync: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  ): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] with ComputeMapping[F] {

        val schema: Schema = loadedSchema
        val QueryType      = schema.ref("Query")
        val BigDecimalType = schema.ref("BigDecimal")
        val LongType       = schema.ref("Long")
        val DurationType   = schema.ref("Duration")
        val PosIntType     = schema.ref("PosInt")

        val typeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                ComputeRoot[List[SpectroscopyResults]]("spectroscopy",
                                                       QueryType,
                                                       spectroscopy[F](environment, redis, itc)
                ),
                ComputeRoot[SpectroscopyGraphResults]("spectroscopyGraphBeta",
                                                      QueryType,
                                                      spectroscopyGraph[F](environment, redis, itc)
                ),
                ComputeRoot[ItcVersions]("versions",
                                         QueryType,
                                         versions[F](environment, redis, itc)
                )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[PosInt](PosIntType),
            LeafMapping[FiniteDuration](DurationType)
          )

        def fallback(a: (IorNec[Problem, Environment], (String, Value))) =
          a._1.addProblem(s"Unexpected param ${a._2._1}")

        override val selectElaborator =
          new SelectElaborator(
            Map(
              QueryType -> {
                case Select("spectroscopy", List(Binding("input", ObjectValue(wv))), child) =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(radialVelocityPartial)
                        .orElse(signalToNoisePartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(instrumentModesPartial)
                        .orElse(constraintsPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopy", Nil, child)))
                case Select("spectroscopyGraphBeta",
                            List(Binding("input", ObjectValue(wv))),
                            child
                    ) =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(radialVelocityPartial)
                        .orElse(exposureTimePartial)
                        .orElse(exposuresPartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(instrumentModePartial)
                        .orElse(constraintsPartial)
                        .orElse(significantFiguresPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopyGraphBeta", Nil, child)))
              }
            )
          )
      }
    }
}
