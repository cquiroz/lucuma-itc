// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats._
import cats.data._
import cats.effect.{ Unique => _, _ }
import cats.syntax.all._
import coulomb._
import coulomb.refined._
import coulomb.si.Kelvin
import coulomb.si._
import coulomb.siprefix._
import coulomb.time._
import edu.gemini.grackle._
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import io.circe.Encoder
import io.circe.Json
import lucuma.core.enum._
import lucuma.core.math.Angle
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.model.Magnitude
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import lucuma.core.syntax.string._
import lucuma.itc.Itc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.itc.search.Result.Spectroscopy
import lucuma.itc.search.SpectroscopyResults
import lucuma.itc.search.TargetProfile
import lucuma.itc.service.syntax.all._
import lucuma.itc.search.syntax.conditions._
import spire.math.Rational

import java.math.RoundingMode
import scala.annotation.nowarn
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Using

import Query._
import Value._
import QueryCompiler._
import lucuma.core.enum.SkyBackground
import lucuma.core.util.Enumerated
import lucuma.core.enum.WaterVapor
import lucuma.itc.ItcObservingConditions

trait Encoders {
  import io.circe.generic.semiauto._
  import io.circe.syntax._
  type Nanosecond  = Nano %* Second
  type Microsecond = Micro %* Second
  type Millisecond = Milli %* Second

  implicit val encoderFiniteDuration: Encoder[FiniteDuration] = new Encoder[FiniteDuration] {
    type Micro
    final def apply(d: FiniteDuration): Json = {
      val value = d.toNanos.withUnit[Nanosecond]
      Json.obj(
        ("microseconds", Json.fromLong(value.toUnit[Microsecond].value)),
        ("milliseconds", Json.fromBigDecimal(value.to[BigDecimal, Millisecond].value)),
        ("seconds", Json.fromBigDecimal(value.to[BigDecimal, Second].value)),
        ("minutes", Json.fromBigDecimal(value.to[BigDecimal, Minute].value)),
        ("hours", Json.fromBigDecimal(value.to[BigDecimal, Hour].value))
      )
    }
  }

  implicit val encoderItcResultSuccess: Encoder[Itc.Result.Success] =
    deriveEncoder[Itc.Result.Success]

  implicit val encoderItcResult: Encoder[Itc.Result] = Encoder.instance {
    case f: Itc.Result.Success          =>
      Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
    case Itc.Result.CalculationError(m) =>
      Json.obj(("resultType", Json.fromString("Error")),
               ("msg", Json.fromString(s"Calculation error $m"))
      )
    case Itc.Result.SourceTooBright(m)  =>
      Json.obj(("resultType", Json.fromString("Error")),
               ("msg", Json.fromString(s"Source too bright $m"))
      )
  }

  implicit val encoderWavelength: Encoder[Wavelength] = new Encoder[Wavelength] {
    final def apply(w: Wavelength): Json = Json.obj(
      ("picometers", Json.fromInt(w.toPicometers.value.value)),
      ("angstrom", Json.fromBigDecimal(w.angstrom.value.toBigDecimal(2, RoundingMode.CEILING))),
      ("nanometers",
       Json.fromBigDecimal(
         w.nanometer.value.toBigDecimal(2, RoundingMode.CEILING)
       )
      ),
      ("micrometers",
       Json.fromBigDecimal(
         w.micrometer.value.toBigDecimal(2, RoundingMode.CEILING)
       )
      )
    )
  }

  implicit val encoderGmosNITCParams: Encoder[GmosNITCParams] =
    deriveEncoder[GmosNITCParams]

  implicit val encoderGmosNorth: Encoder[GmosNorth]                      = new Encoder[GmosNorth] {
    final def apply(a: GmosNorth): Json = Json.obj(
      ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
      ("resolution", Json.fromInt(a.resolution.toInt)),
      ("params", GmosNITCParams(a.disperser, a.fpu, a.filter).asJson),
      ("wavelength", a.λ.asJson)
    )
  }
  implicit val encoderObservingMode: Encoder[ObservingMode.Spectroscopy] = Encoder.instance {
    case gn: GmosNorth => gn.asJson
  }

  implicit val encoderSpectroscopy: Encoder[Spectroscopy] = new Encoder[Spectroscopy] {
    final def apply(s: Spectroscopy): Json = Json.obj(
      ("mode", s.mode.asJson),
      ("itc", s.itc.asJson)
    )
  }

  implicit val encoderSpectroscopyResults: Encoder[SpectroscopyResults] =
    deriveEncoder[SpectroscopyResults]

}

final case class GmosNITCParams(
  disperser: GmosNorthDisperser,
  fpu:       GmosNorthFpu,
  filter:    Option[GmosNorthFilter]
)

object ItcMapping extends Encoders {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F].defer {
      Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
        Schema(src.mkString).right.get
      }.liftTo[F]
    }

  @nowarn
  def basicCase[F[_]: ApplicativeError[*[_], Throwable]](
    itc: Itc[F]
  )(env: Cursor.Env): F[Result[SpectroscopyResults]] =
    (env.get[Wavelength]("wavelength"),
     env.get[Wavelength]("simultaneousCoverage"),
     env.get[Redshift]("redshift"),
     env.get[Rational]("resolution"),
     env.get[types.numeric.PosInt]("signalToNoise"),
     env.get[SpatialProfile]("spatialProfile"),
     env.get[SpectralDistribution]("spectralDistribution"),
     env.get[Magnitude]("magnitude")
    ).traverseN { (wv, sc, rs, r, sn, sp, sd, m) =>
      itc
        .calculate(
          TargetProfile(sp, sd, m, rs),
          ObservingMode.Spectroscopy
            .GmosNorth(wv, GmosNorthDisperser.B480_G5309, GmosNorthFpu.Ifu2Slits, none),
          ItcObservingConditions(
            iq = ImageQuality.OnePointZero,    // Orginially 0.85
            cc = CloudExtinction.OnePointZero, // Originally 0.7
            wv = WaterVapor.Wet,               // Orginally Any
            sb = SkyBackground.Dark,           // Originally 0.5
            airmass = 1.5
          ),
          sn.value
        )
        .map(r =>
          SpectroscopyResults(
            List(
              Spectroscopy(
                ObservingMode.Spectroscopy.GmosNorth(Wavelength.unsafeFromInt(1000),
                                                     GmosNorthDisperser.B480_G5309,
                                                     GmosNorthFpu.Ifu2Slits,
                                                     None
                ),
                r
              )
            )
          ).rightIor[NonEmptyChain[Problem]]
        )
        .handleError { case x =>
          Problem(s"Error calculating itc $x").leftIorNec
        }
    }.map(_.getOrElse((Problem("Error calculating itc")).leftIorNec))

  def spectroscopy[F[_]: ApplicativeError[*[_], Throwable]](
    itc: Itc[F]
  )(env: Cursor.Env): F[Result[List[SpectroscopyResults]]] = {
    println(env)
    println(env.get[Wavelength]("wavelength"))
    println(env.get[Redshift]("redshift"))
    println(env.get[types.numeric.PosInt]("signalToNoise"))
    println(env.get[SpatialProfile]("spatialProfile"))
    println(env.get[SpectralDistribution]("spectralDistribution"))
    println(env.get[Magnitude]("magnitude"))
    println(env.get[List[GmosNITCParams]]("modes"))
    println(env.get[ItcObservingConditions]("constraints"))
    (env.get[Wavelength]("wavelength"),
     env.get[Redshift]("redshift"),
     env.get[types.numeric.PosInt]("signalToNoise"),
     env.get[SpatialProfile]("spatialProfile"),
     env.get[SpectralDistribution]("spectralDistribution"),
     env.get[Magnitude]("magnitude"),
     env.get[List[GmosNITCParams]]("modes"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, sn, sp, sd, m, modes, c) =>
      modes
        .traverse { mode =>
          itc
            .calculate(
              TargetProfile(sp, sd, m, rs),
              ObservingMode.Spectroscopy
                .GmosNorth(wv, mode.disperser, mode.fpu, mode.filter),
              c,
              sn.value
            )
            .handleError { case x =>
              Itc.Result.CalculationError(s"Error calculating itc $x")
            }
            .map(r =>
              SpectroscopyResults(
                List(
                  Spectroscopy(
                    ObservingMode.Spectroscopy.GmosNorth(wv, mode.disperser, mode.fpu, mode.filter),
                    r
                  )
                )
              )
            )
        }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleError { case x =>
          Problem(s"Error calculating itc $x").leftIorNec
        }
    }.map(_.getOrElse((Problem("Missing parameters for spectroscopy")).leftIorNec))
  }

  def parseFwhw(units: List[(String, Value)]): Option[Angle] =
    units.find(_._2 != Value.AbsentValue) match {
      case Some(("microarcseconds", IntValue(n)))   =>
        Angle.microarcseconds.reverseGet(n.toLong).some
      case Some(("milliarcseconds", IntValue(n)))   =>
        Angle.milliarcseconds.reverseGet(n).some
      case Some(("milliarcseconds", FloatValue(n))) =>
        Angle.milliarcseconds.reverseGet(n.toInt).some
      case Some(("arcseconds", IntValue(n)))        =>
        Angle.arcseconds.reverseGet(n).some
      case Some(("arcseconds", FloatValue(n)))      =>
        Angle.arcseconds.reverseGet(n.toInt).some
      case _                                        => None
    }

  def parseWavelength(units: List[(String, Value)]): Option[Wavelength] =
    units.find(_._2 != Value.AbsentValue) match {
      case Some(("picometers", IntValue(n)))    =>
        Wavelength.fromPicometers.getOption(n)
      case Some(("angstroms", IntValue(n)))     =>
        Wavelength.decimalAngstroms.getOption(BigDecimal(n))
      case Some(("angstroms", FloatValue(n)))   =>
        Wavelength.decimalAngstroms.getOption(BigDecimal(n))
      case Some(("nanometers", IntValue(n)))    =>
        Wavelength.decimalNanometers.getOption(BigDecimal(n))
      case Some(("nanometers", FloatValue(n)))  =>
        Wavelength.decimalNanometers.getOption(BigDecimal(n))
      case Some(("micrometers", IntValue(n)))   =>
        Wavelength.decimalMicrometers.getOption(BigDecimal(n))
      case Some(("micrometers", FloatValue(n))) =>
        Wavelength.decimalMicrometers.getOption(BigDecimal(n))
      case _                                    => None
    }

  def apply[F[_]: Sync](itc: Itc[F]): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] with ComputeMapping[F] {

        val schema: Schema         = loadedSchema
        val QueryType              = schema.ref("Query")
        val SpectroscopyResultType = schema.ref("SpectroscopyResult")
        val BigDecimalType         = schema.ref("BigDecimal")
        val LongType               = schema.ref("Long")
        val DurationType           = schema.ref("Duration")
        val typeMappings           =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                ComputeRoot[SpectroscopyResults]("basiccase",
                                                 SpectroscopyResultType,
                                                 basicCase[F](itc)
                ),
                ComputeRoot[List[SpectroscopyResults]]("spectroscopy",
                                                       ListType(SpectroscopyResultType),
                                                       spectroscopy[F](itc)
                )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[FiniteDuration](DurationType)
          )

        def wavelengthPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                               IorNec[Problem, Environment]
        ] = {
          case (i, ("wavelength", ObjectValue(units)))
              if units.filter(_._2 != Value.AbsentValue).length != 1 =>
            val presentUnits =
              units.filter(_._2 != Value.AbsentValue).map(_._1).mkString("{", ", ", "}")
            i.addProblem(s"Wavelength defined with multiple units $presentUnits")
          case (i, ("wavelength", ObjectValue(units))) =>
            val wavelength: Option[Wavelength] = parseWavelength(units)
            wavelength
              .map(w => cursorEnvAdd("wavelength", w)(i))
              .getOrElse(i.addProblem("Wavelength couldn't be parsed"))
        }

        def resolutionPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                               IorNec[Problem, Environment]
        ] = {
          // resolution
          case (i, ("resolution", IntValue(r))) if r > 0 =>
            cursorEnvAdd("resolution", Rational(r))(i)
          case (i, ("resolution", v))                    =>
            i.addProblem(s"Not valid resolution value $v")
        }

        def simultaneousCoveragePartial
          : PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                            IorNec[Problem, Environment]
          ] = {
          // simultaneousCoverage
          case (i, ("simultaneousCoverage", ObjectValue(units)))
              if units.filter(_._2 != Value.AbsentValue).length != 1 =>
            val presentUnits =
              units.filter(_._2 != Value.AbsentValue).map(_._1).mkString("{", ", ", "}")
            i.addProblem(
              s"Simultaneous coverage defined with multiple units $presentUnits"
            )
          case (i, ("simultaneousCoverage", ObjectValue(units))) =>
            val wavelength: Option[Wavelength] = parseWavelength(units)
            wavelength
              .map(w => cursorEnvAdd("simultaneousCoverage", w)(i))
              .getOrElse(i.addProblem("Simultaneous coverage couldn't be parsed"))
        }

        def redshiftPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                             IorNec[Problem, Environment]
        ] = {
          // redshift
          case (i, ("redshift", FloatValue(r))) =>
            val rs = Redshift(r)
            i.map(e => e.copy(env = e.env.add(("redshift", rs))))
          case (i, ("redshift", IntValue(r)))   =>
            val rs = Redshift(r)
            i.map(e => e.copy(env = e.env.add(("redshift", rs))))
          case (i, ("redshift", v))             =>
            i.addLeft(NonEmptyChain.of(Problem(s"Redshift value is not valid $v")))
        }

        def signalToNoisePartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                  IorNec[Problem, Environment]
        ] = {
          // signalToNoise
          case (i, ("signalToNoise", IntValue(r))) if r > 0 =>
            refineV[Positive](r)
              .fold(i.addProblem, v => cursorEnvAdd("signalToNoise", v)(i))
          case (i, ("signalToNoise", v))                    =>
            i.addProblem(s"Not valid signalToNoise value $v")
        }

        def spatialProfilePartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                   IorNec[Problem, Environment]
        ] = {
          // spatialProfile
          case (i, ("spatialProfile", ObjectValue(v))) if v.length === 1 || v.length === 2 =>
            (v match {
              case ("sourceType", TypedEnumValue(EnumValue("POINT_SOURCE", _, _, _))) :: ("fwhm",
                                                                                          AbsentValue
                  ) :: Nil =>
                SpatialProfile.PointSource.some
              case ("sourceType", TypedEnumValue(EnumValue("UNIFORM_SOURCE", _, _, _))) :: ("fwhm",
                                                                                            AbsentValue
                  ) :: Nil =>
                SpatialProfile.UniformSource.some
              case ("sourceType", TypedEnumValue(EnumValue("GAUSSIAN_SOURCE", _, _, _))) :: ("fwhm",
                                                                                             ObjectValue(
                                                                                               fwhm
                                                                                             )
                  ) :: Nil if fwhm.filter(_._2 != Value.AbsentValue).length === 1 =>
                parseFwhw(fwhm).map(SpatialProfile.GaussianSource(_))
              case _ => none
            }).map(sp => cursorEnvAdd("spatialProfile", sp)(i))
              .getOrElse(i.addProblem("Cannot parse spatialProfile"))
          case (i, ("spatialProfile", _))                                                  =>
            i.addProblem("Cannot parse spatialProfile")
        }

        def spectralDistributionPartial
          : PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                            IorNec[Problem, Environment]
          ] = {
          // spectralDistribution
          case (i, ("spectralDistribution", ObjectValue(sd)))
              if sd.filter(_._2 != Value.AbsentValue).length === 1 =>
            sd.filter(_._2 != Value.AbsentValue) match {
              case ("blackBody", ObjectValue(List(("temperature", IntValue(v))))) :: Nil if v > 0 =>
                val blackBody = SpectralDistribution.BlackBody(
                  BigDecimal(v).withRefinedUnit[Positive, Kelvin]
                )
                cursorEnvAdd("spectralDistribution", blackBody)(i)
              case ("blackBody", ObjectValue(List(("temperature", FloatValue(v))))) :: Nil
                  if v > 0 =>
                val blackBody = SpectralDistribution.BlackBody(
                  BigDecimal(v).withRefinedUnit[Positive, Kelvin]
                )
                cursorEnvAdd("spectralDistribution", blackBody)(i)
              case ("powerLaw", ObjectValue(List(("index", IntValue(pl))))) :: Nil if pl > 0      =>
                val powerLaw = SpectralDistribution.PowerLaw(BigDecimal(pl))
                cursorEnvAdd("spectralDistribution", powerLaw)(i)
              case ("powerLaw", ObjectValue(List(("index", FloatValue(pl))))) :: Nil if pl > 0    =>
                val powerLaw = SpectralDistribution.PowerLaw(BigDecimal(pl))
                cursorEnvAdd("spectralDistribution", powerLaw)(i)
              case ("stellar", TypedEnumValue(EnumValue(s, _, _, _))) :: Nil                      =>
                StellarLibrarySpectrum
                  .fromTag(s.fromScreamingSnakeCase)
                  .orElse(StellarLibrarySpectrum.fromTag(s))
                  .map(s =>
                    cursorEnvAdd("spectralDistribution", SpectralDistribution.Library(s.asLeft))(i)
                  )
                  .getOrElse(i.addProblem(s"Unknow stellar library value $s"))
              case ("nonStellar", TypedEnumValue(EnumValue(s, _, _, _))) :: Nil                   =>
                NonStellarLibrarySpectrum
                  .fromTag(s.fromScreamingSnakeCase)
                  .orElse(NonStellarLibrarySpectrum.fromTag(s))
                  .map(s =>
                    cursorEnvAdd("spectralDistribution", SpectralDistribution.Library(s.asRight))(i)
                  )
                  .getOrElse(i.addProblem(s"Unknow stellar library value $s"))
              case _                                                                              =>
                i.addProblem("Cannot parse spatialDistribution")
            }
          case (i, ("spectralDistribution", ObjectValue(sd))) =>
            val v =
              sd.filter(_._2 != Value.AbsentValue).map(_._1).mkString("{", ", ", "}")
            i.addProblem(s"Spectral distribution value is not valid $v")
        }

        def magnitudePartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                              IorNec[Problem, Environment]
        ] = {
          // magnitude
          case (i,
                ("magnitude",
                 ObjectValue(
                   List(("band", TypedEnumValue(EnumValue(band, _, _, _))),
                        ("value", value),
                        ("error", error),
                        ("system", sys)
                   )
                 )
                )
              ) =>
            val b = MagnitudeBand.fromTag(band.fromScreamingSnakeCase)
            val v = value match {
              case IntValue(v)   => MagnitudeValue.fromBigDecimal.getOption(v)
              case FloatValue(v) => MagnitudeValue.fromBigDecimal.getOption(v)
              case _             => none
            }
            val e = error match {
              case IntValue(v)   => MagnitudeValue.fromBigDecimal.getOption(v)
              case FloatValue(v) => MagnitudeValue.fromBigDecimal.getOption(v)
              case _             => none
            }
            val s = sys match {
              case TypedEnumValue(EnumValue(s, _, _, _)) =>
                MagnitudeSystem
                  .fromTag(s.fromScreamingSnakeCase)
                  .orElse(MagnitudeSystem.fromTag(s))
              case UntypedEnumValue(s)                   =>
                MagnitudeSystem
                  .fromTag(s.fromScreamingSnakeCase)
                  .orElse(MagnitudeSystem.fromTag(s))
              case _                                     => none
            }
            (v, b, s)
              .mapN(Magnitude(_, _, e, _))
              .map(cursorEnvAdd("magnitude", _)(i))
              .getOrElse(i.addProblem("Cannot parse magnitude"))
        }

        def instrumentModesPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                    IorNec[Problem, Environment]
        ] = { case (i, ("modes", ListValue(m))) =>
          val modes = m.collect { case ObjectValue(List(("gmosN", gmosN))) =>
            gmosN match {
              case ObjectValue(
                    List(("disperser", TypedEnumValue(EnumValue(d, _, _, _))),
                         ("customMask", AbsentValue),
                         ("fpu", TypedEnumValue(EnumValue(fpu, _, _, _))),
                         ("filter", TypedEnumValue(EnumValue(f, _, _, _)))
                    )
                  ) =>
                (GmosNorthDisperser
                   .fromTag(d.fromScreamingSnakeCase)
                   .orElse(GmosNorthDisperser.fromTag(d)),
                 GmosNorthFpu
                   .fromTag(fpu.fromScreamingSnakeCase)
                   .orElse(GmosNorthFpu.fromTag(fpu))
                   .orElse(
                     GmosNorthFpu.all.find(
                       _.tag.toLowerCase.replace("_", "") === fpu.toLowerCase.replace("_", "")
                     )
                   )
                ).mapN(
                  GmosNITCParams(_,
                                 _,
                                 GmosNorthFilter
                                   .fromTag(f.fromScreamingSnakeCase)
                                   .orElse(GmosNorthFilter.fromTag(f))
                  )
                )
              case _ => none
            }
          }.flatten
          cursorEnvAdd("modes", modes)(i)
        }

        def constraintsPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                IorNec[Problem, Environment]
        ] = {
          case (i,
                ("constraints",
                 ObjectValue(
                   List(("imageQuality", TypedEnumValue(EnumValue(iq, _, _, _))),
                        ("cloudExtinction", TypedEnumValue(EnumValue(ce, _, _, _))),
                        ("skyBackground", TypedEnumValue(EnumValue(sb, _, _, _))),
                        ("waterVapor", TypedEnumValue(EnumValue(wv, _, _, _)))
                   )
                 )
                )
              ) =>
            (iqFromTag(iq.fromScreamingSnakeCase)
               .orElse(iqFromTag(iq)),
             ceFromTag(ce.fromScreamingSnakeCase)
               .orElse(ceFromTag(ce)),
             (Enumerated[WaterVapor].all.find(_.label.toScreamingSnakeCase === wv)),
             Enumerated[SkyBackground].all.find(_.label.equalsIgnoreCase(sb))
            ).mapN((iq, ce, wv, sb) =>
              cursorEnvAdd("constraints", ItcObservingConditions(iq, ce, wv, sb, 1))(i)
            ).getOrElse(i.addProblem("Cannot parse constraints"))
        }

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
                        .orElse(redshiftPartial)
                        .orElse(signalToNoisePartial)
                        .orElse(spatialProfilePartial)
                        .orElse(spectralDistributionPartial)
                        .orElse(magnitudePartial)
                        .orElse(instrumentModesPartial)
                        .orElse(constraintsPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopy", Nil, child)))
                case Select("basiccase", List(Binding("input", ObjectValue(wv))), child)    =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(simultaneousCoveragePartial)
                        .orElse(resolutionPartial)
                        .orElse(signalToNoisePartial)
                        .orElse(spatialProfilePartial)
                        .orElse(spectralDistributionPartial)
                        .orElse(redshiftPartial)
                        .orElse(magnitudePartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )

                  }.map(e => e.copy(child = Select("basiccase", Nil, child)))
              }
            )
          )
      }
    }
}
