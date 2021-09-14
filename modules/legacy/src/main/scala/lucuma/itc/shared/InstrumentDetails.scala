package lucuma.itc.shared

import cats.data._
import cats.syntax.all._
import lucuma.itc.model.syntax.disperser._
// import edu.gemini.spModel.core.Site
// import edu.gemini.spModel.core.{Wavelength, Site}
// import edu.gemini.spModel.data.YesNoType
// import edu.gemini.spModel.gemini.acqcam.AcqCamParams
// import edu.gemini.spModel.gemini.altair.AltairParams
// import edu.gemini.spModel.gemini.flamingos2.Flamingos2
// import edu.gemini.spModel.gemini.gmos.{GmosSouthType, GmosNorthType, GmosCommonType}
// import edu.gemini.spModel.gemini.gnirs.GNIRSParams
// import edu.gemini.spModel.gemini.gsaoi.Gsaoi
// import edu.gemini.spModel.gemini.michelle.MichelleParams
// import edu.gemini.spModel.gemini.nifs.NIFSParams
// import edu.gemini.spModel.gemini.niri.Niri
// import edu.gemini.spModel.gemini.trecs.TReCSParams
import lucuma.core.enum
import lucuma.core.math.Wavelength
import lucuma.core.math.Angle
import lucuma.core.math.syntax.int._

/*
 * A collection of objects that define subsets of instrument configuration parameters
 * which are needed for ITC calculations of the corresponding instruments.
 */

sealed trait InstrumentDetails

// final case class AcquisitionCamParameters(
//                      colorFilter:       AcqCamParams.ColorFilter,
//                      ndFilter:          AcqCamParams.NDFilter) extends InstrumentDetails
//
// final case class Flamingos2Parameters(
//                      filter:            Flamingos2.Filter,
//                      grism:             Flamingos2.Disperser,
//                      mask:              Flamingos2.FPUnit,
//                      customSlitWidth:   Option[Flamingos2.CustomSlitWidth],
//                      readMode:          Flamingos2.ReadMode) extends InstrumentDetails
//
// // TODO-GHOSTITC
// final case class GhostParameters() extends InstrumentDetails
//
sealed trait GmosParameters[D] extends InstrumentDetails {
  def centralWavelength: Wavelength
  def ampGain:           enum.GmosAmpGain
  def ampReadMode:       enum.GmosAmpReadMode
  def customSlitWidth:   Option[enum.GmosCustomSlitWidth]
  def spatialBinning:    Int
  def spectralBinning:   Int
  def builtinROI:        enum.GmosRoi
  def site:              enum.Site
  def ccdType:           D
  def filterFileName: Option[String]
  def slitWidth: Option[Angle]
  def addFilter: Boolean
}

final case class GmosSouthParameters(
                     filterGrating:            Ior[enum.GmosSouthFilter, enum.GmosSouthDisperser],
                     centralWavelength: Wavelength,
                     fpMask:            enum.GmosSouthFpu,
                     ampGain:           enum.GmosAmpGain,
                     ampReadMode:       enum.GmosAmpReadMode,
                     customSlitWidth:   Option[enum.GmosCustomSlitWidth],
                     spatialBinning:    Int,
                     spectralBinning:   Int,
                     ccdType:           enum.GmosSouthDetector,
                     builtinROI:        enum.GmosRoi) extends GmosParameters[enum.GmosSouthDetector] {
  val site = enum.Site.GS
  val filterFileName: Option[String] = filterGrating.leftMap(_.longName).onlyLeft
  def slitWidth = if (fpMask.isIFU) 300.milliarcseconds.some
    else if (customSlitWidth.isDefined) customSlitWidth.map(_.width)
    else fpMask.slitWidth
}

final case class GmosNorthParameters(
                     filterGrating:            Ior[enum.GmosNorthFilter, enum.GmosNorthDisperser],
                     centralWavelength: Wavelength,
                     fpMask:            enum.GmosNorthFpu,
                     ampGain:           enum.GmosAmpGain,
                     ampReadMode:       enum.GmosAmpReadMode,
                     customSlitWidth:   Option[enum.GmosCustomSlitWidth],
                     spatialBinning:    Int,
                     spectralBinning:   Int,
                     ccdType:           enum.GmosNorthDetector,
                     builtinROI:        enum.GmosRoi) extends GmosParameters[enum.GmosNorthDetector] {
  val site = enum.Site.GN
  val filterFileName: Option[String] = filterGrating.leftMap(_.longName).onlyLeft
  def slitWidth = if (fpMask.isIFU) 300.milliarcseconds.some
    else if (customSlitWidth.isDefined) customSlitWidth.map(_.width)
    else fpMask.slitWidth
}
//
// final case class GnirsParameters(
//                      pixelScale:        GNIRSParams.PixelScale,
//                      filter:            Option[GNIRSParams.Filter],
//                      grating:           Option[GNIRSParams.Disperser],
//                      readMode:          GNIRSParams.ReadMode,
//                      crossDispersed:    GNIRSParams.CrossDispersed,
//                      centralWavelength: Wavelength,
//                      slitWidth:         GNIRSParams.SlitWidth,
//                      camera:            Option[GNIRSParams.Camera],
//                      wellDepth:         GNIRSParams.WellDepth,
//                      altair:            Option[AltairParameters]) extends InstrumentDetails
//
// final case class GsaoiParameters(
//                      filter:            Gsaoi.Filter,
//                      readMode:          Gsaoi.ReadMode,
//                      largeSkyOffset:    Int,
//                      gems:              GemsParameters) extends InstrumentDetails
//
// final case class MichelleParameters(
//                      filter:            MichelleParams.Filter,
//                      grating:           MichelleParams.Disperser,
//                      centralWavelength: Wavelength,
//                      mask:              MichelleParams.Mask,
//                      polarimetry:       YesNoType) extends InstrumentDetails
//
// final case class NifsParameters(
//                      filter:            NIFSParams.Filter,
//                      grating:           NIFSParams.Disperser,
//                      readMode:          NIFSParams.ReadMode,
//                      centralWavelength: Wavelength,
//                      altair:            Option[AltairParameters]) extends InstrumentDetails
//
// final case class NiriParameters(
//                      filter:            Niri.Filter,
//                      grism:             Niri.Disperser,
//                      camera:            Niri.Camera,
//                      readMode:          Niri.ReadMode,
//                      wellDepth:         Niri.WellDepth,
//                      mask:              Niri.Mask,
//                      builtinROI:        Niri.BuiltinROI,
//                      altair:            Option[AltairParameters]) extends InstrumentDetails
//
// final case class TRecsParameters(
//                      filter:            TReCSParams.Filter,
//                      instrumentWindow:  TReCSParams.WindowWheel,
//                      grating:           TReCSParams.Disperser,
//                      centralWavelength: Wavelength,
//                      mask:              TReCSParams.Mask) extends InstrumentDetails
//

// == AO

// final case class AltairParameters(
//                      guideStarSeparation: Double,
//                      guideStarMagnitude:  Double,
//                      fieldLens:           AltairParams.FieldLens,
//                      wfsMode:             AltairParams.GuideStarType)
//
// final case class GemsParameters(
//                      avgStrehl:           Double,
//                      strehlBand:          String)
//

object InstrumentDetails {

  // NOTE: This is similar to the code in ItcUniqueConfig which decides on imaging or spectroscopy setup
  // on Config elements. There should be a way to share this?

  // figure out if the instrument is configured for imaging by checking if a disperser element is present
  def isImaging(i: InstrumentDetails): Boolean = i match {
    // case _: AcquisitionCamParameters  => true                                       // Acq cam is imaging only
    // case i: Flamingos2Parameters      => i.grism.equals(Flamingos2.Disperser.NONE)
    // case i: GnirsParameters           => i.grating.isEmpty
    // case _: GsaoiParameters           => true                                       // Gsaoi is imaging only
    // case i: MichelleParameters        => i.grating.equals(MichelleParams.Disperser.MIRROR)
    // case _: NifsParameters            => false                                      // NIFS is spectroscopy only
    // case _: GhostParameters           => true // TBD is this true?
    // case i: NiriParameters            => i.grism.equals(Niri.Disperser.NONE)
    // case i: TRecsParameters           => i.grating.equals(TReCSParams.Disperser.MIRROR)
    case _: GmosSouthParameters            =>
      true
    case _: GmosNorthParameters            =>
      true
      // i.grating.equals(GmosNorthType.DisperserNorth.MIRROR) ||
      // i.grating.equals(GmosSouthType.DisperserSouth.MIRROR)
  }

  // figure out if the instrument is configured for spectroscopy (ie. not imaging)
  def isSpectroscopy(i: InstrumentDetails): Boolean = !isImaging(i)

}
