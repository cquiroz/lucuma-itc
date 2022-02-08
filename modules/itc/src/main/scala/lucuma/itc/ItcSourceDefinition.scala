// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.enum.SurfaceBrightness
import lucuma.core.enum._
import lucuma.core.math.Angle
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Redshift
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import lucuma.itc.search.TargetProfile

final case class ItcSourceDefinition(
  profile:      SpatialProfile,
  distribution: SpectralDistribution, // Switch to SpectralDistribution
  norm:         MagnitudeValue,
  units:        Either[MagnitudeSystem, SurfaceBrightness],
  normBand:     MagnitudeBand,
  redshift:     Redshift
)

object ItcSourceDefinition {

  def fromTargetProfile(p: TargetProfile): ItcSourceDefinition =
    ItcSourceDefinition(
      p.spatialProfile,
      p.spectralDistribution,
      p.magnitude.value,
      p.spatialProfile match {
        case SpatialProfile.GaussianSource(_) => Left(p.magnitude.system)
        case SpatialProfile.PointSource       => Left(p.magnitude.system)
        case SpatialProfile.UniformSource     =>
          Right {
            p.magnitude.system match {
              case MagnitudeSystem.Vega           => SurfaceBrightness.Vega
              case MagnitudeSystem.AB             => SurfaceBrightness.AB
              case MagnitudeSystem.Jy             => SurfaceBrightness.Jy
              case MagnitudeSystem.Watts          => SurfaceBrightness.Watts
              case MagnitudeSystem.ErgsFrequency  => SurfaceBrightness.ErgsFrequency
              case MagnitudeSystem.ErgsWavelength => SurfaceBrightness.ErgsWavelength
            }
          }
      },
      p.magnitude.band,
      p.redshift
    )

  implicit val spatialProfileEncoder: Encoder[SpatialProfile] =
    new Encoder[SpatialProfile] {
      import SpatialProfile._
      def apply(a: SpatialProfile): Json =
        a match {
          case PointSource           => Json.obj("PointSource" -> Json.obj())
          case UniformSource         => Json.obj("UniformSource" -> Json.obj())
          case g @ GaussianSource(_) =>
            Json.obj(
              "GaussianSource" -> Json.obj(
                "fwhm" -> Angle.signedDecimalArcseconds.get(g.fwhm).asJson
              )
            )
        }
    }

  implicit val spectralDistributionEncoder: Encoder[SpectralDistribution] =
    new Encoder[SpectralDistribution] {
      import SpectralDistribution._
      def apply(a: SpectralDistribution): Json =
        a match {
          case BlackBody(t)       =>
            Json.obj(
              "BlackBody" -> Json.obj(
                "temperature" -> Json.fromDoubleOrNull(t.value.value.toDouble)
              )
            )
          case PowerLaw(i)        =>
            Json.obj("PowerLaw" -> Json.obj("index" -> Json.fromDoubleOrNull(i.toDouble)))
          case Library(Left(s))   =>
            Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
          case Library(Right(ns)) =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(ns.ocs2Tag)))
        }
    }

  implicit val unitEncoder: Encoder[Either[MagnitudeSystem, SurfaceBrightness]] =
    new Encoder[Either[MagnitudeSystem, SurfaceBrightness]] {
      def apply(a: Either[MagnitudeSystem, SurfaceBrightness]): Json =
        a match {
          case Left(ms)  => Json.obj("MagnitudeSystem" -> Json.fromString(ms.tag))
          case Right(sb) => Json.obj("SurfaceBrightness" -> Json.fromString(sb.ocs2Tag))
        }
    }

  implicit val magnitudeValueEncoder: Encoder[MagnitudeValue] =
    Encoder[BigDecimal].contramap(MagnitudeValue.fromBigDecimal.reverseGet)

  implicit val magnitudeBandEncoder: Encoder[MagnitudeBand] =
    Encoder[String].contramap(_.shortName)

  implicit val redshiftEncoder: Encoder[Redshift] =
    Encoder.forProduct1("z")(_.z)

  implicit val encoder: Encoder[ItcSourceDefinition] =
    deriveEncoder

}
