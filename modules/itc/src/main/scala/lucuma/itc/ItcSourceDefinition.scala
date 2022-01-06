// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

// import coulomb.define.UnitDefinition
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.enum._
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.syntax.sed._
import lucuma.core.math.BrightnessValue
import lucuma.core.model.SourceProfile
import lucuma.core.model.UnnormalizedSED
import lucuma.core.math.dimensional.Measure
import lucuma.core.model.SpectralDefinition

final case class ItcSourceDefinition(
  profile:  SourceProfile,
  normBand: Band,
  redshift: Redshift
)

object ItcSourceDefinition {

  def fromTargetProfile(p: TargetProfile): ItcSourceDefinition =
    ItcSourceDefinition(
      p.sourceProfile,
      p.band,
      p.redshift
    )

  implicit val sourceProfileEncoder: Encoder[SourceProfile] =
    new Encoder[SourceProfile] {
      import SourceProfile._
      def apply(a: SourceProfile): Json =
        a match {
          case _: Point    =>
            Json.obj("PointSource" -> Json.obj(), "units" -> Json.fromString("AAAAA"))
          case _: Uniform  => Json.obj("UniformSource" -> Json.obj())
          case g: Gaussian =>
            Json.obj(
              "GaussianSource" -> Json.obj(
                "fwhm" -> Angle.signedDecimalArcseconds.get(g.fwhm).asJson
              )
            )
        }
    }

  implicit val spectralDistributionEncoder: Encoder[UnnormalizedSED] =
    new Encoder[UnnormalizedSED] {
      import UnnormalizedSED._
      def apply(a: UnnormalizedSED): Json =
        a match {
          case BlackBody(t)       =>
            Json.obj(
              "BlackBody" -> Json.obj(
                "temperature" -> Json.fromDoubleOrNull(t.value.value.toDouble)
              )
            )
          case PowerLaw(i)        =>
            Json.obj("PowerLaw" -> Json.obj("index" -> Json.fromDoubleOrNull(i.toDouble)))
          case StellarLibrary(s)  =>
            Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
          case Galaxy(s)          =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case Planet(s)          =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case HIIRegion(s)       =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case PlanetaryNebula(s) =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case Quasar(s)          =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case s: CoolStarModel   =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case _                  => // TODO CoolStar and UserDefined
            Json.obj("Library" -> Json.Null)
        }
    }

  // implicit val unitEncoder: Encoder[UnitDefinition]             = ???
  // implicit val unitEncoder: Encoder[Either[MagnitudeSystem, SurfaceBrightness]] =
  //   new Encoder[Either[MagnitudeSystem, SurfaceBrightness]] {
  //     def apply(a: Either[MagnitudeSystem, SurfaceBrightness]): Json =
  //       a match {
  //         case Left(ms)  => Json.obj("MagnitudeSystem" -> Json.fromString(ms.tag))
  //         case Right(sb) => Json.obj("SurfaceBrightness" -> Json.fromString(sb.ocs2Tag))
  //       }
  //   }
  //
  implicit val brightnessValueEncoder: Encoder[BrightnessValue] =
    Encoder[BigDecimal].contramap(BrightnessValue.fromBigDecimal.reverseGet)

  implicit val bandEncoder: Encoder[Band] =
    Encoder[String].contramap(_.shortName)

  implicit val redshiftEncoder: Encoder[Redshift] =
    Encoder.forProduct1("z")(_.z)

  implicit val encoder: Encoder[ItcSourceDefinition] =
    // deriveEncoder[ItcSourceDefinition]
    new Encoder[ItcSourceDefinition] {
      def apply(s: ItcSourceDefinition): Json = {
        val source = s.profile match {
          case _: SourceProfile.Point    =>
            Json.obj("PointSource" -> Json.obj())
          case _: SourceProfile.Uniform  => Json.obj("UniformSource" -> Json.obj())
          case g: SourceProfile.Gaussian =>
            Json.obj(
              "GaussianSource" -> Json.obj(
                "fwhm" -> Angle.signedDecimalArcseconds.get(g.fwhm).asJson
              )
            )
        }

        val units: Json = s.profile match {
          case SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses))
              if brightnesses.contains(s.normBand) =>
            brightnesses.get(s.normBand).map(_.units.serialized) match {
              case Some("VEGA_MAGNITUDE") => Json.obj("MagnitudeSystem" -> Json.fromString("Vega"))
              case r                      => println(r); Json.Null
            }
          case _ => Json.Null
        }

        val distribution = s.profile match {
          case SourceProfile.Point(SpectralDefinition.BandNormalized(sed, _)) =>
            sed.asJson
          // case SourceProfile.Uniform(sed, _)  =>
          //   sed.toJson
          // case SourceProfile.Gaussian(sed, _) =>
          // sed.toJson
        }

        Json.obj("profile"      -> source,
                 "normBand"     -> s.normBand.asJson,
                 "norm"         -> Json.fromInt(5),
                 "redshift"     -> s.redshift.asJson,
                 "units"        -> units,
                 "distribution" -> distribution
        )
      }
      // Json.fromString("ITC")
    }

}
