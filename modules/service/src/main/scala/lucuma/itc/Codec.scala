// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.{Applicative}
import cats.effect.Concurrent
// import cats.syntax.all._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.generic.extras.semiauto.deriveEnumerationDecoder
import org.http4s._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl._
import lucuma.itc.model._
import coulomb.refined._
import eu.timepit.refined.numeric.Positive
import lucuma.core.math.Angle
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import org.typelevel.log4cats.Logger
import edu.gemini.itc.shared.TelescopeDetails
import edu.gemini.spModel.telescope.IssPort
import edu.gemini.spModel.guide.GuideProbe

trait ItcParametersCodec {
  import lucuma.core.math.Redshift
  import lucuma.core.math.Wavelength
  import lucuma.core.enum.MagnitudeBand
  import lucuma.core.model.SpatialProfile

  implicit val redshiftDefinitionDecoder: Decoder[Redshift] =
    deriveDecoder[Redshift]
  implicit val magnitudeBandDecoder: Decoder[MagnitudeBand] =
    deriveDecoder[MagnitudeBand]
  implicit val magnitudeBandEncoder: Encoder[MagnitudeBand] =
    deriveEncoder[MagnitudeBand]
  implicit val wavelengthDecoder: Decoder[Wavelength] =
    Decoder.decodeInt.emap(x => Either.fromOption(Wavelength.fromNanometers(x), "Failure"))
  // implicit val brigTnessUnitDecoder: Decoder[BrightnessUnit] = ???
  implicit val wavelengethEncoder: Encoder[Wavelength] =
    Encoder.encodeInt.contramap[Wavelength](_.toPicometers.value.value)
  implicit val widthDecoder: Decoder[EmissionLine.Width] =
    Decoder.decodeBigDecimal.emap(x => Either.catchNonFatal(x.withRefinedUnit[Positive, EmissionLine.KPS]).leftMap(_.getMessage))
  implicit val widthEncoder: Encoder[EmissionLine.Width] =
    Encoder.encodeBigDecimal.contramap[EmissionLine.Width](_.value.value)
    // Decoder.decodeBigDecimal.emap(x => Either.catchNonFatal(x.withRefinedUnit[Positive, EmissionLine.KPS]).leftMap(_.getMessage))
  implicit val irradianceDecoder: Decoder[EmissionLine.Irradiance] =
    Decoder.decodeBigDecimal.emap(x => Either.catchNonFatal(x.withRefinedUnit[Positive, EmissionLine.WSM]).leftMap(_.getMessage))
  implicit val irEncoder: Encoder[EmissionLine.Irradiance] =
    Encoder.encodeBigDecimal.contramap[EmissionLine.Irradiance](_.value.value)
  implicit val spectralIrradianceDecoder: Decoder[EmissionLine.SpectralIrradiance] =
    Decoder.decodeBigDecimal.emap(x => Either.catchNonFatal(x.withRefinedUnit[Positive, EmissionLine.WSMM]).leftMap(_.getMessage))
  implicit val sirEncoder: Encoder[EmissionLine.SpectralIrradiance] =
    Encoder.encodeBigDecimal.contramap[EmissionLine.SpectralIrradiance](_.value.value)
  implicit val emissionDecoder: Decoder[EmissionLine] =
    deriveDecoder[EmissionLine]
  implicit val powerLawDecoder: Decoder[PowerLaw] =
    deriveDecoder[PowerLaw]
  implicit val userDefinedSpectrumDecoder: Decoder[UserDefinedSpectrum] =
    deriveDecoder[UserDefinedSpectrum]
  implicit val spectralDistrubutionDecoder: Decoder[SpectralDistribution] =
    deriveDecoder[SpectralDistribution]
  implicit val elEncoder: Encoder[EmissionLine] =
    deriveEncoder[EmissionLine]
  implicit val pwEncoder: Encoder[PowerLaw] =
    deriveEncoder[PowerLaw]
  implicit val udEncoder: Encoder[UserDefinedSpectrum] =
    deriveEncoder[UserDefinedSpectrum]
  implicit val spectralDistrubutionEncoder: Encoder[SpectralDistribution] =
    deriveEncoder[SpectralDistribution]
  implicit val angleDecoder: Decoder[Angle] =
    Decoder.decodeLong.emap(x => Angle.microarcseconds.reverseGet(x).asRight)
  implicit val gaussianDecoder: Decoder[SpatialProfile.GaussianSource] =
    deriveDecoder[SpatialProfile.GaussianSource]
  implicit val uniformSourceDecoder: Decoder[SpatialProfile.UniformSource.type] =
    deriveDecoder[SpatialProfile.UniformSource.type]
  implicit val pointSourceDecoder: Decoder[SpatialProfile.PointSource.type] =
    deriveDecoder[SpatialProfile.PointSource.type]
  implicit val spatialProfileDecoder: Decoder[SpatialProfile] =
    List[Decoder[SpatialProfile]](
      Decoder[SpatialProfile.PointSource.type].widen,
      Decoder[SpatialProfile.UniformSource.type].widen,
    ).reduceLeft(_ or _)
  implicit val sourceDefinitionDecoder: Decoder[SourceDefinition] =
    deriveDecoder[SourceDefinition]
  implicit val waterVaporDecoder: Decoder[WaterVapor] =
    deriveEnumerationDecoder[WaterVapor]
  implicit val cloudCoverDecoder: Decoder[CloudExtinction] =
    deriveEnumerationDecoder[CloudExtinction]
  implicit val imageQualityDecoder: Decoder[ImageQuality] =
    deriveEnumerationDecoder[ImageQuality]
  implicit val skyBackgroundDecoder: Decoder[SkyBackground] =
    deriveEnumerationDecoder[SkyBackground]
  implicit val observingConditionsDecoder: Decoder[ObservingConditions] =
    deriveDecoder[ObservingConditions]
  implicit val s2NDecoder: Decoder[S2NMethod] =
    deriveDecoder[S2NMethod]
  implicit val calculationMethodDecoder: Decoder[CalculationMethod] =
    List[Decoder[CalculationMethod]](
      Decoder[S2NMethod].widen,
    ).reduceLeft(_ or _)
  implicit val autoApertureDecoder: Decoder[AutoAperture] =
    deriveDecoder[AutoAperture]
  implicit val autoApertureEncoder: Encoder[AutoAperture] =
    deriveEncoder[AutoAperture]
  implicit val userApertureDecoder: Decoder[UserAperture] =
    deriveDecoder[UserAperture]
  implicit val userApertureEncoder: Encoder[UserAperture] =
    deriveEncoder[UserAperture]
  implicit val apertureMethodDecoder: Decoder[ApertureMethod] =
    List[Decoder[ApertureMethod]](
      Decoder[AutoAperture].widen,
      Decoder[UserAperture].widen,
    ).reduceLeft(_ or _)
  implicit val apertureMethodEncoder: Encoder[ApertureMethod] = Encoder.instance {
    case a: AutoAperture => a.asJson
    case a: UserAperture => a.asJson
  }
  implicit val analysisMethodDecoder: Decoder[AnalysisMethod] =
    List[Decoder[AnalysisMethod]](
      Decoder[ApertureMethod].widen,
    ).reduceLeft(_ or _)
  implicit val analysisMethodEncoder: Encoder[AnalysisMethod] = Encoder.instance {
    case a: ApertureMethod => a.asJson
  }
  implicit val telescopeDetailsDecoder: Decoder[TelescopeDetails] = new Decoder[TelescopeDetails] {
    final def apply(c: HCursor): Decoder.Result[TelescopeDetails] =
      new TelescopeDetails(TelescopeDetails.Coating.SILVER, IssPort.SIDE_LOOKING, GuideProbe.Type.PWFS).asRight
  }

  // import edu.gemini.spModel.gemini.gmos.{GmosSouthType, GmosNorthType, GmosCommonType}
  // implicit val gmosDisperserDecoder: Decoder[GmosCommonType.Disperser] = Decoder.decodeString.emap {
  //   case "R831_G5302" => GmosNorthType.DisperserNorth.R831_G5302.asRight
  //   case a => a.asLeft
  // }
  // implicit val royDecoder: Decoder[GmosCommonType.BuiltinROI] = Decoder.decodeString.emap {
  //   case "FULL_FRAME" => GmosCommonType.BuiltinROI.FULL_FRAME.asRight
  //   case a => a.asLeft
  // }
  // import edu.gemini.spModel.core.Site
  // implicit val siteDecoder: Decoder[Site] = Decoder.decodeString.emap{
  //   case "GN" => Site.GN.asRight
  //   case "GS" => Site.GS.asRight
  //   case a => a.asLeft
  // }
  implicit val gmosParameterDetailsDecoder: Decoder[GmosParameters] =
    deriveDecoder[GmosParameters]
  implicit val observingDetailsDecoder: Decoder[ObservationDetails] =
    deriveDecoder[ObservationDetails]
  implicit val instrumentDetailsDecoder: Decoder[InstrumentDetails] =
    deriveDecoder[InstrumentDetails]
  implicit val paramsDecoder: Decoder[ItcParameters] =
    deriveDecoder[ItcParameters]
}

// #service
