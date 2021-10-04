// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats._
import cats.data._
import cats.effect.{Unique => _, _}
import cats.syntax.all._
import coulomb.refined._
import coulomb.si.Kelvin
import edu.gemini.grackle._
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import io.circe.Encoder
import io.circe.Json
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.MagnitudeBand
import lucuma.core.enum.MagnitudeSystem
import lucuma.core.enum.NonStellarLibrarySpectrum
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.math.Angle
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.model.Magnitude
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import lucuma.itc.Itc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.itc.search.Result.Spectroscopy
import lucuma.itc.search.SpectroscopyResults
import lucuma.itc.service.syntax.all._

import java.math.RoundingMode
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Using

import Query._
import Value._
import QueryCompiler._

trait Encoders {
  import io.circe.generic.semiauto._
  import io.circe.syntax._
  implicit val encoderFiniteDuration: Encoder[FiniteDuration] = new Encoder[FiniteDuration] {
    final def apply(d: FiniteDuration): Json = Json.obj(
      ("seconds", Json.fromLong(d.toSeconds))
    )
  }

  implicit val encoderItcResultSuccess: Encoder[Itc.Result.Success] =
    deriveEncoder[Itc.Result.Success]

  implicit val encoderItcResult: Encoder[Itc.Result] = Encoder.instance {
    case f: Itc.Result.Success         =>
      Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
    case Itc.Result.SourceTooBright(m) =>
      Json.obj(("resultType", Json.fromString("Error")),
               ("message", Json.fromString(s"Source too bright $m"))
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

  implicit val encoderGmosNorth: Encoder[GmosNorth]                      = new Encoder[GmosNorth] {
    final def apply(a: GmosNorth): Json = Json.obj(
      ("instrument", Json.fromString(a.instrument.toString)),
      ("resolution", Json.fromInt(a.resolution.toInt)),
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

object ItcMapping extends Encoders {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F].defer {
      Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
        Schema(src.mkString).right.get
      }.liftTo[F]
    }

  def computeItc[F[_]: Applicative](env: Cursor.Env): F[Result[SpectroscopyResults]] = {
    println(env.get[Wavelength]("wavelength"))
    println(env.get[Wavelength]("simultaneousCoverage"))
    println(env.get[Redshift]("redshift"))
    println(env.get[types.numeric.PosInt]("resolution"))
    println(env.get[types.numeric.PosInt]("signalToNoise"))
    println(env.get[SpatialProfile]("spatialProfile"))
    println(env.get[SpectralDistribution]("spectralDistribution"))
    println(env.get[Magnitude]("magnitude"))
    println(env)
    SpectroscopyResults(
      List(
        Spectroscopy(
          ObservingMode.Spectroscopy.GmosNorth(Wavelength.unsafeFromInt(1000),
                                               GmosNorthDisperser.B480_G5309,
                                               GmosNorthFpu.Ifu2Slits,
                                               None
          ),
          Itc.Result.Success(1.seconds, 10, 10)
        )
      )
    ).rightIor.pure[F]
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

  def apply[F[_]: Sync]: F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] with ComputeMapping[F] {

        val schema: Schema         = loadedSchema
        val QueryType              = schema.ref("Query")
        val SpectroscopyResultType = schema.ref("spectroscopyResult")
        val BigDecimalType         = schema.ref("BigDecimal")

        val typeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                ComputeRoot[SpectroscopyResults]("spectroscopy",
                                                 SpectroscopyResultType,
                                                 computeItc[F]
                )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType)
          )

        override val selectElaborator =
          new SelectElaborator(
            Map(QueryType -> {
              case Select("spectroscopy", List(Binding("input", ObjectValue(wv))), child) =>
                wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                  // wavelength
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

                  // simultaneousCoverage
                  case (i, ("simultaneousCoverage", ObjectValue(units)))
                      if units.filter(_._2 != Value.AbsentValue).length != 1 =>
                    val presentUnits =
                      units.filter(_._2 != Value.AbsentValue).map(_._1).mkString("{", ", ", "}")
                    i.addProblem(s"Simultaneous coverage defined with multiple units $presentUnits")
                  case (i, ("simultaneousCoverage", ObjectValue(units))) =>
                    val wavelength: Option[Wavelength] = parseWavelength(units)
                    wavelength
                      .map(w => cursorEnvAdd("simultaneousCoverage", w)(i))
                      .getOrElse(i.addProblem("Simultaneous coverage couldn't be parsed"))

                  // resolution
                  case (i, ("resolution", IntValue(r))) if r > 0 =>
                    refineV[Positive](r)
                      .fold(i.addProblem, v => cursorEnvAdd("resolution", v)(i))
                  case (i, ("resolution", v))                    =>
                    i.addProblem(s"Not valid resolution value $v")

                  // signalToNoise
                  case (i, ("signalToNoise", IntValue(r))) if r > 0 =>
                    refineV[Positive](r)
                      .fold(i.addProblem, v => cursorEnvAdd("signalToNoise", v)(i))
                  case (i, ("signalToNoise", v))                    =>
                    i.addProblem(s"Not valid signalToNoise value $v")

                  // spatialProfile
                  case (i, ("spatialProfile", ObjectValue(v)))
                      if v.length === 1 || v.length === 2 =>
                    (v.sortBy(_._1) match {
                      case ("fwhm", AbsentValue) :: ("sourceType",
                                                     TypedEnumValue(
                                                       EnumValue("POINT_SOURCE", _, _, _)
                                                     )
                          ) :: Nil =>
                        SpatialProfile.PointSource.some
                      case ("fwhm", AbsentValue) :: ("sourceType",
                                                     TypedEnumValue(
                                                       EnumValue("UNIFORM_SOURCE", _, _, _)
                                                     )
                          ) :: Nil =>
                        SpatialProfile.UniformSource.some
                      case ("fwhm", ObjectValue(fwhm)) :: ("sourceType",
                                                           TypedEnumValue(
                                                             EnumValue("GAUSSIAN_SOURCE", _, _, _)
                                                           )
                          ) :: Nil if fwhm.filter(_._2 != Value.AbsentValue).length === 1 =>
                        parseFwhw(fwhm).map(SpatialProfile.GaussianSource(_))
                      case _ => none
                    }).map(sp => cursorEnvAdd("spatialProfile", sp)(i))
                      .getOrElse(i.addProblem("Cannot parse spatialProfile"))
                  case (i, ("spatialProfile", _)) =>
                    i.addProblem("Cannot parse spatialProfile")

                  // spectralDistribution
                  case (i, ("spectralDistribution", ObjectValue(sd)))
                      if sd.filter(_._2 != Value.AbsentValue).length === 1 =>
                    sd.filter(_._2 != Value.AbsentValue) match {
                      case ("blackBody", ObjectValue(List(("temperature", IntValue(v))))) :: Nil
                          if v > 0 =>
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
                      case ("powerLaw", ObjectValue(List(("index", IntValue(pl))))) :: Nil
                          if pl > 0 =>
                        val powerLaw = SpectralDistribution.PowerLaw(BigDecimal(pl))
                        cursorEnvAdd("spectralDistribution", powerLaw)(i)
                      case ("powerLaw", ObjectValue(List(("index", FloatValue(pl))))) :: Nil
                          if pl > 0 =>
                        val powerLaw = SpectralDistribution.PowerLaw(BigDecimal(pl))
                        cursorEnvAdd("spectralDistribution", powerLaw)(i)
                      case ("stellar", TypedEnumValue(EnumValue(s, _, _, _))) :: Nil    =>
                        StellarLibrarySpectrum
                          .fromTag(s.fromScreamingSnakeCase)
                          .orElse(StellarLibrarySpectrum.fromTag(s))
                          .map(s => cursorEnvAdd("spectralDistribution", s)(i))
                          .getOrElse(i.addProblem(s"Unknow stellar library value $s"))
                      case ("nonStellar", TypedEnumValue(EnumValue(s, _, _, _))) :: Nil =>
                        NonStellarLibrarySpectrum
                          .fromTag(s.fromScreamingSnakeCase)
                          .orElse(NonStellarLibrarySpectrum.fromTag(s))
                          .map(s => cursorEnvAdd("spectralDistribution", s)(i))
                          .getOrElse(i.addProblem(s"Unknow stellar library value $s"))
                      case _                                                            =>
                        i.addProblem("Cannot parse spatialDistribution")
                    }
                  case (i, ("spectralDistribution", ObjectValue(sd))) =>
                    val v = sd.filter(_._2 != Value.AbsentValue).map(_._1).mkString("{", ", ", "}")
                    i.addProblem(s"Spectral distribution value is not valid $v")

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

                  // redshift
                  case (i, ("redshift", FloatValue(r))) =>
                    val rs = Redshift(r)
                    i.map(e => e.copy(env = e.env.add(("redshift", rs))))
                  case (i, ("redshift", IntValue(r)))   =>
                    val rs = Redshift(r)
                    i.map(e => e.copy(env = e.env.add(("redshift", rs))))
                  case (i, ("redshift", v))             =>
                    i.addLeft(NonEmptyChain.of(Problem(s"Redshift value is not valid $v")))
                  case (e, _)                           => e
                }.map(e => e.copy(child = Select("spectroscopy", Nil, child)))
            })
          )
      }
    }
}
