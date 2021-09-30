// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats._
import cats.syntax.all._
import cats.data._

import edu.gemini.grackle._
import edu.gemini.grackle.circe.CirceMapping

import Query._, Value._
import QueryCompiler._
import io.circe.Json
import io.circe.Encoder
import scala.util.Using
import scala.io.Source
import cats.effect.{ Unique => _, _ }
import lucuma.odb.search.SpectroscopyResults
import lucuma.odb.search.Result.Spectroscopy
import lucuma.odb.search.ObservingMode
import lucuma.odb.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.odb.itc.Itc
import lucuma.core.math.Wavelength
import scala.concurrent.duration._
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosNorthFpu
import java.math.RoundingMode
import lucuma.core.math.Redshift
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import lucuma.core.math.Angle
import lucuma.core.model.SpatialProfile

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
    case f: Itc.Result.Success =>
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

  implicit val encoderGmosNorth: Encoder[GmosNorth] = new Encoder[GmosNorth] {
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
  implicit class MoreOptionOps[A](self: Option[A]) {
    def toRightIorNec[E](e: => E): Ior[NonEmptyChain[E], A] =
      self match {
        case Some(a) => Ior.Right(a)
        case None    => Ior.Left(NonEmptyChain.of(e))
      }
  }

  implicit class MoreIdOps[A](self: A) {

    def leftIorNec[B]: Ior[NonEmptyChain[A], B] =
      Ior.Left(NonEmptyChain.of(self))
  }

  implicit class IorOps[A](self: IorNec[Problem, A]) {
    def addProblem(problem: String): Ior[NonEmptyChain[Problem], A] =
      self.addLeft(NonEmptyChain.of(Problem(problem)))
  }

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
    println(env.get[Redshift]("redshift"))
    println(env.get[types.numeric.PosInt]("resolution"))
    println(env.get[types.numeric.PosInt]("signalToNoise"))
    println(env.get[SpatialProfile]("spatialProfile"))
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
      case Some(("microarcseconds", IntValue(n))) =>
        Angle.microarcseconds.reverseGet(n.toLong).some
      case Some(("milliarcseconds", IntValue(n))) =>
        Angle.milliarcseconds.reverseGet(n).some
      case Some(("milliarcseconds", FloatValue(n))) =>
        Angle.milliarcseconds.reverseGet(n.toInt).some
      case Some(("arcseconds", IntValue(n))) =>
        Angle.arcseconds.reverseGet(n).some
      case Some(("arcseconds", FloatValue(n))) =>
        Angle.arcseconds.reverseGet(n.toInt).some
      case _ => None
    }

  def parseWavelength(units: List[(String, Value)]): Option[Wavelength] =
    units.find(_._2 != Value.AbsentValue) match {
      case Some(("picometers", IntValue(n))) =>
        Wavelength.fromPicometers.getOption(n)
      case Some(("angstroms", IntValue(n))) =>
        Wavelength.decimalAngstroms.getOption(BigDecimal(n))
      case Some(("angstroms", FloatValue(n))) =>
        Wavelength.decimalAngstroms.getOption(BigDecimal(n))
      case Some(("nanometers", IntValue(n))) =>
        Wavelength.decimalNanometers.getOption(BigDecimal(n))
      case Some(("nanometers", FloatValue(n))) =>
        Wavelength.decimalNanometers.getOption(BigDecimal(n))
      case Some(("micrometers", IntValue(n))) =>
        Wavelength.decimalMicrometers.getOption(BigDecimal(n))
      case Some(("micrometers", FloatValue(n))) =>
        Wavelength.decimalMicrometers.getOption(BigDecimal(n))
      case _ => None
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
                      .map { w =>
                        i.map(e =>
                          e.copy(env = e.env.add(("wavelength", w)),
                                 Select("spectroscopy", Nil, child)
                          )
                        )
                      }
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
                      .map { w =>
                        i.map(e =>
                          e.copy(env = e.env.add(("simultaneousCoverage", w)),
                                 Select("spectroscopy", Nil, child)
                          )
                        )
                      }
                      .getOrElse(i.addProblem("Simultaneous coverage couldn't be parsed"))

                  // resolution
                  case (i, ("resolution", IntValue(r))) if r > 0 =>
                    refineV[Positive](r)
                      .fold(i.addProblem,
                            v => i.map(e => e.copy(env = e.env.add(("resolution", v))))
                      )
                  case (i, ("resolution", v)) =>
                    i.addProblem(s"Not valid resolution value $v")

                  // signalToNoise
                  case (i, ("signalToNoise", IntValue(r))) if r > 0 =>
                    refineV[Positive](r)
                      .fold(i.addProblem,
                            v => i.map(e => e.copy(env = e.env.add(("signalToNoise", v))))
                      )
                  case (i, ("signalToNoise", v)) =>
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
                    }).map(sp => i.map(e => e.copy(env = e.env.add(("spatialProfile", sp)))))
                      .getOrElse(i.addProblem("Cannot parse spatialProfile"))

                  case (i, ("spatialProfile", _)) =>
                    i.addProblem("Cannot parse spatialProfile")

                  case (i, ("spectralDistribution", v)) =>
                    println(v)
                    i.addProblem("Cannot parse spectralDistribution")
                  // redshift
                  case (i, ("redshift", FloatValue(r))) =>
                    val rs = Redshift(r)
                    i.map(e => e.copy(env = e.env.add(("redshift", rs))))
                  case (i, ("redshift", IntValue(r))) =>
                    val rs = Redshift(r)
                    i.map(e => e.copy(env = e.env.add(("redshift", rs))))
                  case (i, ("redshift", v)) =>
                    i.addLeft(NonEmptyChain.of(Problem(s"Redshift value is not valid $v")))
                  case (e, _) => e
                }
            })
          )
      }
    }
}
