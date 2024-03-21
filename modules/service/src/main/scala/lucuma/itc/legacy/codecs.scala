// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.string.*
import lucuma.itc.ChartType
import lucuma.itc.ItcChart
import lucuma.itc.ItcChartGroup
import lucuma.itc.ItcObservingConditions
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.legacy.syntax.all.*
import lucuma.itc.search.*
import lucuma.itc.search.ObservingMode.SpectroscopyMode.*
import lucuma.itc.syntax.all.given

import java.math.MathContext
import scala.util.Try

////////////////////////////////////////////////////////////
//
// These are encoders/decoders used to communicate with the
// old ocs2-based itc
//
////////////////////////////////////////////////////////////
private def toItcAirmass(m: Double): Double =
  if (m <= 1.35) 1.2 else if (m <= 1.75) 1.5 else 2.0

given Encoder[ItcObservingConditions] =
  import lucuma.itc.legacy.syntax.conditions.*
  Encoder.forProduct5("exactiq", "exactcc", "wv", "sb", "airmass") { a =>
    (Json.obj(
       "arcsec"     -> Json.fromBigDecimal(
         a.iq.toArcSeconds.value.toBigDecimal(MathContext.DECIMAL32)
       )
     ),
     Json.obj(
       "extinction" -> Json.fromBigDecimal(BigDecimal(a.cc.toBrightness))
     ),
     a.wv.ocs2Tag,
     a.sb.ocs2Tag,
     toItcAirmass(a.airmass)
    )
  }

private val encodeGmosNorthSpectroscopy: Encoder[ObservingMode.SpectroscopyMode.GmosNorth] =
  new Encoder[ObservingMode.SpectroscopyMode.GmosNorth] {
    def apply(a: ObservingMode.SpectroscopyMode.GmosNorth): Json =
      Json.obj(
        // Translate observing mode to OCS2 style
        "centralWavelength" -> Json.fromString(
          s"${Wavelength.decimalNanometers.reverseGet(a.λ)} nm"
        ),
        "filter"            -> Json.obj(
          "FilterNorth" -> a.filter.fold[Json](Json.fromString("NONE"))(r =>
            Json.fromString(r.ocs2Tag)
          )
        ),
        "grating"           -> Json.obj("DisperserNorth" -> Json.fromString(a.disperser.ocs2Tag)),
        "fpMask"            -> Json.obj("FPUnitNorth" -> Json.fromString(a.fpu.builtin.ocs2Tag)),
        "spectralBinning"   -> Json.fromInt(a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.One).count),
        "site"              -> Json.fromString("GN"),
        "ccdType"           -> Json.fromString("HAMAMATSU"),
        "ampReadMode"       -> Json.fromString(
          a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
        ),
        "builtinROI"        -> Json.fromString(
          a.roi.getOrElse(GmosRoi.FullFrame).tag.toScreamingSnakeCase
        ),
        "spatialBinning"    -> Json.fromInt(a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.One).count),
        "customSlitWidth"   -> Json.Null,
        "ampGain"           -> Json.fromString(
          a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
        )
      )
  }

private val encodeGmosNorthImaging: Encoder[ObservingMode.ImagingMode.GmosNorth] =
  new Encoder[ObservingMode.ImagingMode.GmosNorth] {
    def apply(a: ObservingMode.ImagingMode.GmosNorth): Json =
      Json.obj(
        // Translate observing mode to OCS2 style
        "centralWavelength" -> Json.fromString(
          s"${Wavelength.decimalNanometers.reverseGet(a.λ)} nm"
        ),
        "filter"            -> Json.obj(
          "FilterNorth" ->
            Json.fromString(a.filter.ocs2Tag)
        ),
        "grating"           -> Json.obj("DisperserNorth" -> "MIRROR".asJson),
        "fpMask"            -> Json.obj("FPUnitNorth" -> "FPU_NONE".asJson),
        "spectralBinning"   -> Json.fromInt(a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.Two).count),
        "site"              -> Json.fromString("GN"),
        "ccdType"           -> Json.fromString("HAMAMATSU"),
        "ampReadMode"       -> Json.fromString(
          a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
        ),
        "builtinROI"        -> Json.fromString("FULL_FRAME"),
        "spatialBinning"    -> Json.fromInt(a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.Two).count),
        "customSlitWidth"   -> Json.Null,
        "ampGain"           -> Json.fromString(
          a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
        )
      )
  }

private val encodeGmosSouthSpectroscopy: Encoder[ObservingMode.SpectroscopyMode.GmosSouth] =
  new Encoder[ObservingMode.SpectroscopyMode.GmosSouth] {
    def apply(a: ObservingMode.SpectroscopyMode.GmosSouth): Json =
      Json.obj(
        // Translate observing mode to OCS2 style
        "centralWavelength" -> Json.fromString(
          s"${Wavelength.decimalNanometers.reverseGet(a.λ)} nm"
        ),
        "filter"            -> Json.obj(
          "FilterSouth" -> a.filter.fold[Json](Json.fromString("NONE"))(r =>
            Json.fromString(r.ocs2Tag)
          )
        ),
        "grating"           -> Json.obj("DisperserSouth" -> Json.fromString(a.disperser.ocs2Tag)),
        "fpMask"            -> Json.obj("FPUnitSouth" -> Json.fromString(a.fpu.builtin.ocs2Tag)),
        "spectralBinning"   -> Json.fromInt(a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.One).count),
        "site"              -> Json.fromString("GS"),
        "ccdType"           -> Json.fromString("HAMAMATSU"),
        "ampReadMode"       -> Json.fromString(
          a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
        ),
        "builtinROI"        -> Json.fromString(
          a.roi.getOrElse(GmosRoi.FullFrame).tag.toScreamingSnakeCase
        ),
        "spatialBinning"    -> Json.fromInt(a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.One).count),
        "customSlitWidth"   -> Json.Null,
        "ampGain"           -> Json.fromString(
          a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
        )
      )
  }

private val encodeGmosSouthImaging: Encoder[ObservingMode.ImagingMode.GmosSouth] =
  new Encoder[ObservingMode.ImagingMode.GmosSouth] {
    def apply(a: ObservingMode.ImagingMode.GmosSouth): Json =
      Json.obj(
        // Translate observing mode to OCS2 style
        "centralWavelength" -> Json.fromString(
          s"${Wavelength.decimalNanometers.reverseGet(a.λ)} nm"
        ),
        "filter"            -> Json.obj(
          "FilterSouth" ->
            Json.fromString(a.filter.ocs2Tag)
        ),
        "grating"           -> Json.obj("DisperserSouth" -> "MIRROR".asJson),
        "fpMask"            -> Json.obj("FPUnitSouth" -> "FPU_NONE".asJson),
        "spectralBinning"   -> Json.fromInt(a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.Two).count),
        "site"              -> Json.fromString("GS"),
        "ccdType"           -> Json.fromString("HAMAMATSU"),
        "ampReadMode"       -> Json.fromString(
          a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
        ),
        "builtinROI"        -> Json.fromString("FULL_FRAME"),
        "spatialBinning"    -> Json.fromInt(a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.Two).count),
        "customSlitWidth"   -> Json.Null,
        "ampGain"           -> Json.fromString(
          a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
        )
      )
  }

private given Encoder[ItcInstrumentDetails] = (a: ItcInstrumentDetails) =>
  a.mode match
    case a: ObservingMode.SpectroscopyMode.GmosNorth =>
      Json.obj("GmosParameters" -> encodeGmosNorthSpectroscopy(a))
    case a: ObservingMode.SpectroscopyMode.GmosSouth =>
      Json.obj("GmosParameters" -> encodeGmosSouthSpectroscopy(a))
    case a: ObservingMode.ImagingMode.GmosNorth      =>
      Json.obj("GmosParameters" -> encodeGmosNorthImaging(a))
    case a: ObservingMode.ImagingMode.GmosSouth      =>
      Json.obj("GmosParameters" -> encodeGmosSouthImaging(a))

private given Encoder[ItcWavefrontSensor] = Encoder[String].contramap(_.ocs2Tag)

private given Encoder[ItcTelescopeDetails] = (a: ItcTelescopeDetails) =>
  Json.obj(
    "mirrorCoating"  -> Json.fromString("SILVER"),
    "instrumentPort" -> Json.fromString("SIDE_LOOKING"),
    "wfs"            -> a.wfs.asJson
  )

private given Encoder[SourceProfile] = (a: SourceProfile) =>
  import SourceProfile._
  a match {
    case _: Point    =>
      Json.obj("PointSource" -> Json.obj())
    case _: Uniform  => Json.obj("UniformSource" -> Json.obj())
    case g: Gaussian =>
      Json.obj(
        "GaussianSource" -> Json.obj(
          "fwhm" -> Angle.signedDecimalArcseconds.get(g.fwhm).asJson
        )
      )
  }

private given Encoder[UnnormalizedSED] = (a: UnnormalizedSED) =>
  import UnnormalizedSED._
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
    case s: CoolStarModel   =>
      Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
    case PlanetaryNebula(s) =>
      Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
    case Galaxy(s)          =>
      Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
    case Planet(s)          =>
      Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
    case HIIRegion(s)       =>
      Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
    case Quasar(s)          =>
      Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
    case _                  => // TODO UserDefined
      Json.obj("Library" -> Json.Null)
  }

private given Encoder[Band] =
  Encoder[String].contramap(_.shortName)

private given Encoder[Redshift] =
  Encoder.forProduct1("z")(_.z)

given Encoder[ItcSourceDefinition] = (s: ItcSourceDefinition) =>
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
        case Some("VEGA_MAGNITUDE")                  => Json.obj("MagnitudeSystem" -> Json.fromString("Vega"))
        case Some("AB_MAGNITUDE")                    => Json.obj("MagnitudeSystem" -> Json.fromString("AB"))
        case Some("JANSKY")                          => Json.obj("MagnitudeSystem" -> Json.fromString("Jy"))
        case Some("W_PER_M_SQUARED_PER_UM")          =>
          Json.obj("MagnitudeSystem" -> Json.fromString("W/m²/µm"))
        case Some("ERG_PER_S_PER_CM_SQUARED_PER_A")  =>
          Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Å"))
        case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ") =>
          Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Hz"))
        case _                                       =>
          Json.Null
      }
    // FIXME Support emision lines
    // case SourceProfile.Point(SpectralDefinition.EmissionLines(_, brightnesses)) =>
    //   Json.Null
    //     if brightnesses.contains(s.normBand) =>
    //   brightnesses.get(s.normBand).map(_.units.serialized) match {
    //     case Some("VEGA_MAGNITUDE")                  => Json.obj("MagnitudeSystem" -> Json.fromString("Vega"))
    //     case Some("AB_MAGNITUDE")                    => Json.obj("MagnitudeSystem" -> Json.fromString("AB"))
    //     case Some("JANSKY")                          => Json.obj("MagnitudeSystem" -> Json.fromString("Jy"))
    //     case Some("W_PER_M_SQUARED_PER_UM")          =>
    //       Json.obj("MagnitudeSystem" -> Json.fromString("W/m²/µm"))
    //     case Some("ERG_PER_S_PER_CM_SQUARED_PER_A")  =>
    //       Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Å"))
    //     case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ") =>
    //       Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Hz"))
    //     case _                                       =>
    //       Json.Null
    //   }
    case SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))
        if brightnesses.contains(s.normBand) =>
      brightnesses.get(s.normBand).map(_.units.serialized) match {
        case Some("VEGA_MAG_PER_ARCSEC_SQUARED")                        =>
          Json.obj("SurfaceBrightness" -> Json.fromString("Vega mag/arcsec²"))
        case Some("AB_MAG_PER_ARCSEC_SQUARED")                          =>
          Json.obj("SurfaceBrightness" -> Json.fromString("AB mag/arcsec²"))
        case Some("JY_PER_ARCSEC_SQUARED")                              =>
          Json.obj("SurfaceBrightness" -> Json.fromString("Jy/arcsec²"))
        case Some("W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED")          =>
          Json.obj("SurfaceBrightness" -> Json.fromString("W/m²/µm/arcsec²"))
        case Some("ERG_PER_S_PER_CM_SQUARED_PER_A_PER_ARCSEC_SQUARED")  =>
          Json.obj("SurfaceBrightness" -> Json.fromString("erg/s/cm²/Å/arcsec²"))
        case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ_PER_ARCSEC_SQUARED") =>
          Json.obj("SurfaceBrightness" -> Json.fromString("erg/s/cm²/Hz/arcsec²"))
        case _                                                          =>
          Json.Null
      }
    case SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
        if brightnesses.contains(s.normBand) =>
      brightnesses.get(s.normBand).map(_.units.serialized) match {
        case Some("VEGA_MAGNITUDE")                  => Json.obj("MagnitudeSystem" -> Json.fromString("Vega"))
        case Some("AB_MAGNITUDE")                    => Json.obj("MagnitudeSystem" -> Json.fromString("AB"))
        case Some("JANSKY")                          => Json.obj("MagnitudeSystem" -> Json.fromString("Jy"))
        case Some("W_PER_M_SQUARED_PER_UM")          =>
          Json.obj("MagnitudeSystem" -> Json.fromString("W/m²/µm"))
        case Some("ERG_PER_S_PER_CM_SQUARED_PER_A")  =>
          Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Å"))
        case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ") =>
          Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Hz"))
        case _                                       =>
          Json.Null
      }

    // FIXME Support emision lines
    case _ => Json.Null
  }

  val value: Json = s.profile match {
    case SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses))
        if brightnesses.contains(s.normBand) =>
      brightnesses
        .get(s.normBand)
        .map(_.value)
        .asJson
    case SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))
        if brightnesses.contains(s.normBand) =>
      brightnesses
        .get(s.normBand)
        .map(_.value)
        .asJson
    case SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
        if brightnesses.contains(s.normBand) =>
      brightnesses
        .get(s.normBand)
        .map(_.value)
        .asJson
    // FIXME: Handle emission line
    case _ => Json.Null
  }

  val distribution = s.profile match {
    case SourceProfile.Point(SpectralDefinition.BandNormalized(sed, _))       =>
      sed.asJson
    // FIXME support emmision lines
    // case SourceProfile.Point(SpectralDefinition.EmissionLines(sed, _))        =>
    //   Json.Null
    case SourceProfile.Uniform(SpectralDefinition.BandNormalized(sed, _))     =>
      sed.asJson
    case SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(sed, _)) =>
      sed.asJson
    // FIXME: Handle emission line
    case _                                                                    => Json.Null
  }

  Json.obj("profile"      -> source,
           "normBand"     -> s.normBand.asJson,
           "norm"         -> value,
           "redshift"     -> s.redshift.asJson,
           "units"        -> units,
           "distribution" -> distribution
  )

given Encoder[ItcParameters] =
  deriveEncoder[ItcParameters]

private given Decoder[SeriesDataType] = (c: HCursor) =>
  Decoder.decodeJsonObject(c).flatMap { str =>
    val key = str.keys.headOption.orEmpty
    Try(SeriesDataType.valueOf(key)).toEither.leftMap { _ =>
      DecodingFailure(s"no enum value matched for $key", List(CursorOp.Field(key)))
    }
  }

private given Decoder[ChartType] = (c: HCursor) =>
  Decoder.decodeJsonObject(c).flatMap { str =>
    val key = str.keys.headOption.orEmpty
    Try(ChartType.valueOf(key)).toEither.leftMap { _ =>
      DecodingFailure(s"no enum value matched for $key", List(CursorOp.Field(key)))
    }
  }

private given Decoder[ItcSeries] = (c: HCursor) =>
  for
    title <- c.downField("title").as[String]
    dt    <- c.downField("dataType").as[SeriesDataType]
    data  <- c.downField("data")
               .as[List[List[Double]]]
               .map { i =>
                 (i.lift(0), i.lift(1)) match
                   case (Some(a), Some(b)) if a.length === b.length => a.zip(b)
                   case _                                           => List.empty
               }
  yield ItcSeries(title, dt, data)

given Decoder[ItcChart] = (c: HCursor) =>
  for
    series <- c.downField("series").as[List[ItcSeries]]
    d      <- c.downField("chartType").as[ChartType]
  yield ItcChart(d, series)

given Decoder[ItcChartGroup] = (c: HCursor) =>
  c.downField("charts").as[NonEmptyList[ItcChart]].map(ItcChartGroup.apply)

given Decoder[GraphsRemoteResult] = (c: HCursor) =>
  for
    charts <- (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
                .downField("chartGroups")
                .as[NonEmptyList[ItcChartGroup]]
    ccd    <- (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
                .downField("ccds")
                .as[NonEmptyList[ItcRemoteCcd]]
  yield GraphsRemoteResult(ccd, charts)

given Decoder[ExposureCalculation] = (c: HCursor) =>
  for
    time  <- c.downField("exposureTime").as[Double]
    count <- c
               .downField("exposures")
               .as[Int]
               .flatMap(refineV[Positive](_).leftMap(e => DecodingFailure(e, List.empty)))
    sn    <-
      c
        .downField("signalToNoise")
        .as[BigDecimal]
        .flatMap(s =>
          SignalToNoise.FromBigDecimalRounding
            .getOption(s)
            .toRight(DecodingFailure(s"Invalid s/n value $s", Nil))
        )
  yield ExposureCalculation(time, count, sn)

given Decoder[ExposureTimeRemoteResult] = (c: HCursor) =>
  val spec: Decoder.Result[ExposureTimeRemoteResult] =
    c.downField("ItcSpectroscopyResult")
      .downField("exposureCalculation")
      .as[ExposureCalculation]
      .map(c => ExposureTimeRemoteResult(NonEmptyList.one(c)))

  val img: Decoder.Result[ExposureTimeRemoteResult] =
    c.downField("ItcImagingResult")
      .downField("exposureCalculation")
      .as[NonEmptyList[ExposureCalculation]]
      .map(c => ExposureTimeRemoteResult(c))
  spec.orElse(img)
