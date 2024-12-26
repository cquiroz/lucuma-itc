// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display.*
import lucuma.core.syntax.string.*
import lucuma.itc.GraphType
import lucuma.itc.ItcGraph
import lucuma.itc.ItcGraphGroup
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

given Encoder[Wavelength] = w =>
  Json.fromString:
    s"${Wavelength.decimalNanometers.reverseGet(w)} nm"

private val encodeGmosNorthSpectroscopy: Encoder[ObservingMode.SpectroscopyMode.GmosNorth] =
  new Encoder[ObservingMode.SpectroscopyMode.GmosNorth] {
    def apply(a: ObservingMode.SpectroscopyMode.GmosNorth): Json =
      Json.obj(
        // Translate observing mode to OCS2 style
        "centralWavelength" -> a.centralWavelength.asJson,
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
        "centralWavelength" -> a.centralWavelength.asJson,
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
        "centralWavelength" -> a.centralWavelength.asJson,
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
        "centralWavelength" -> a.centralWavelength.asJson,
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
    case Point(_)          =>
      Json.obj("PointSource" -> Json.obj())
    case Uniform(_)        => Json.obj("UniformSource" -> Json.obj())
    case Gaussian(fwhm, _) =>
      Json.obj(
        "GaussianSource" -> Json.obj(
          "fwhm" -> Angle.signedDecimalArcseconds.get(fwhm).asJson
        )
      )
  }

private given Encoder[UnnormalizedSED] = (a: UnnormalizedSED) =>
  import UnnormalizedSED.*
  a match
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

private given Encoder[Band] =
  Encoder[String].contramap(_.shortName)

private given Encoder[Redshift] =
  Encoder.forProduct1("z")(_.z)

private def encodeEmissionLine[T](
  wavelength:           Wavelength,
  lineWidth:            LineWidthQuantity,
  lineflux:             LineFluxMeasure[T],
  fluxDensityContinuum: FluxDensityContinuumMeasure[T]
): Json =
  Json.obj(
    "EmissionLine" -> Json.obj(
      "wavelength" -> wavelength.asJson,
      "width"      -> lineWidth.toMeasure.shortName.asJson,
      "flux"       -> lineflux.exact.shortName.asJson,
      "continuum"  -> fluxDensityContinuum.exact.shortName.asJson
    )
  )

given Encoder[ItcSourceDefinition] = (s: ItcSourceDefinition) =>
  val source: Json = s.sourceProfile match
    case SourceProfile.Point(_)          =>
      Json.obj("PointSource" -> Json.obj())
    case SourceProfile.Uniform(_)        => Json.obj("UniformSource" -> Json.obj())
    case SourceProfile.Gaussian(fwhm, _) =>
      Json.obj(
        "GaussianSource" -> Json.obj(
          "fwhm" -> Angle.signedDecimalArcseconds.get(fwhm).asJson
        )
      )

  val units: Json = (s.bandOrLine, s.sourceProfile) match
    case (Left(band), SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses)))   =>
      brightnesses
        .get(band)
        .map: b =>
          Json.obj("MagnitudeSystem" -> b.units.abbv.stripSuffix(" mag").asJson)
        .getOrElse(Json.Null)
    case (Left(band), SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))) =>
      brightnesses
        .get(band)
        .map: b =>
          Json.obj("SurfaceBrightness" -> b.units.abbv.asJson)
        .getOrElse(Json.Null)
    case (Left(band),
          SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
        ) =>
      brightnesses
        .get(band)
        .map: b =>
          Json.obj("MagnitudeSystem" -> b.units.abbv.stripSuffix(" mag").asJson)
        .getOrElse(Json.Null)
    case (Right(_),
          SourceProfile.Point(SpectralDefinition.EmissionLines(_, fluxDensityContinuum))
        ) =>
      Json.obj("MagnitudeSystem" -> fluxDensityContinuum.units.abbv.asJson)
    case (Right(_),
          SourceProfile.Uniform(SpectralDefinition.EmissionLines(_, fluxDensityContinuum))
        ) =>
      Json.obj("SurfaceBrightness" -> fluxDensityContinuum.units.abbv.asJson)
    case (Right(_),
          SourceProfile.Gaussian(_, SpectralDefinition.EmissionLines(_, fluxDensityContinuum))
        ) =>
      Json.obj("MagnitudeSystem" -> fluxDensityContinuum.units.abbv.asJson)
    case _                                                                                       =>
      Json.Null

  val value: Json = (s.bandOrLine, s.sourceProfile) match
    case (Left(band), SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses)))   =>
      brightnesses
        .get(band)
        .map(_.value)
        .asJson
    case (Left(band), SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))) =>
      brightnesses
        .get(band)
        .map(_.value)
        .asJson
    case (Left(band),
          SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
        ) =>
      brightnesses
        .get(band)
        .map(_.value)
        .asJson
    case _                                                                                       =>
      Json.Null

  val distribution: Json = (s.bandOrLine, s.sourceProfile) match
    case (Left(band), SourceProfile.Point(SpectralDefinition.BandNormalized(sed, _)))       =>
      sed.asJson
    case (Right(wavelength),
          SourceProfile.Point(SpectralDefinition.EmissionLines(lines, fluxDensityContinuum))
        ) =>
      lines
        .get(wavelength)
        .map: line =>
          encodeEmissionLine(wavelength, line.lineWidth, line.lineFlux, fluxDensityContinuum)
        .getOrElse(Json.Null)
    case (Left(band), SourceProfile.Uniform(SpectralDefinition.BandNormalized(sed, _)))     =>
      sed.asJson
    case (Right(wavelength),
          SourceProfile.Uniform(SpectralDefinition.EmissionLines(lines, fluxDensityContinuum))
        ) =>
      lines
        .get(wavelength)
        .map: line =>
          encodeEmissionLine(wavelength, line.lineWidth, line.lineFlux, fluxDensityContinuum)
        .getOrElse(Json.Null)
    case (Left(band), SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(sed, _))) =>
      sed.asJson
    case (Right(wavelength),
          SourceProfile.Gaussian(_, SpectralDefinition.EmissionLines(lines, fluxDensityContinuum))
        ) =>
      lines
        .get(wavelength)
        .map: line =>
          encodeEmissionLine(wavelength, line.lineWidth, line.lineFlux, fluxDensityContinuum)
        .getOrElse(Json.Null)
    case _                                                                                  =>
      Json.Null

  val normBand: Json = // Use a dummy value in case of emission lines
    s.bandOrLine.fold(_.asJson, _ => (Band.R: Band).asJson)

  Json.obj(
    "profile"      -> source,
    "normBand"     -> normBand,
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

private given Decoder[GraphType] = (c: HCursor) =>
  val byLegacyKey: Map[String, GraphType] = Map(
    "SignalChart"      -> GraphType.SignalGraph,
    "SignalPixelChart" -> GraphType.SignalPixelGraph,
    "S2NChart"         -> GraphType.S2NGraph
  )

  Decoder.decodeJsonObject(c).flatMap { str =>
    val key: String = str.keys.headOption.orEmpty
    byLegacyKey
      .get(key)
      .toRight:
        DecodingFailure(s"no enum value matched for $key", List(CursorOp.Field(key)))
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

given Decoder[ItcGraph] = (c: HCursor) =>
  for
    series <- c.downField("series").as[List[ItcSeries]]
    d      <- c.downField("chartType").as[GraphType]
  yield ItcGraph(d, series)

given Decoder[ItcGraphGroup] = (c: HCursor) =>
  c.downField("charts").as[NonEmptyChain[ItcGraph]].map(ItcGraphGroup.apply)

given Decoder[GraphsRemoteResult] = (c: HCursor) =>
  for
    graphs <- (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
                .downField("chartGroups")
                .as[NonEmptyChain[ItcGraphGroup]]
    ccd    <- (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
                .downField("ccds")
                .as[NonEmptyChain[ItcRemoteCcd]]
  yield GraphsRemoteResult(ccd, graphs)

given Decoder[ExposureCalculation] = (c: HCursor) =>
  println(c.value)
  for
    tsn   <- // We attempt decoding and validating S/N first
      c
        .downField("totalSignalToNoise")
        .as[BigDecimal]
        .flatMap: s =>
          SignalToNoise.FromBigDecimalRounding
            .getOption(s)
            .filter(_.toBigDecimal > BigDecimal(0))
            .toRight:
              DecodingFailure(
                s"Invalid S/N value computed: $s",
                c.downField("totalSignalToNoise").history
              )
    ssn   <- // We attempt decoding and validating S/N first
      c
        .downField("singleSignalToNoise")
        .as[BigDecimal]
        .flatMap: s =>
          SignalToNoise.FromBigDecimalRounding
            .getOption(s)
            .filter(_.toBigDecimal > BigDecimal(0))
            .toRight:
              DecodingFailure(
                s"Invalid S/N value computed: $s",
                c.downField("singleSignalToNoise").history
              )
    time  <- c.downField("exposureTime").as[Double]
    count <-
      c
        .downField("exposures")
        .as[Int]
        .flatMap:
          refineV[Positive](_).leftMap(e => DecodingFailure(e, c.downField("exposures").history))
  yield ExposureCalculation(time, count, tsn, ssn)

given Decoder[IntegrationTimeRemoteResult] = (c: HCursor) =>
  val spec: Option[Decoder.Result[IntegrationTimeRemoteResult]] =
    c.downField("ItcSpectroscopyResult")
      .downField("exposureCalculation")
      .success
      .map:
        _.as[ExposureCalculation]
          .map(c => IntegrationTimeRemoteResult(NonEmptyChain.one(c)))

  val img: Decoder.Result[IntegrationTimeRemoteResult] =
    c.downField("ItcImagingResult")
      .downField("exposureCalculation")
      .as[NonEmptyChain[ExposureCalculation]]
      .map(c => IntegrationTimeRemoteResult(c))

  spec.getOrElse(img)
