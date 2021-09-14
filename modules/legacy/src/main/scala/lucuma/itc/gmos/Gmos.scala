package lucuma.itc.gmos

import lucuma.itc.base.Instrument
import lucuma.itc.base.SpectroscopyInstrument
import lucuma.itc.base.BinningProvider
import lucuma.itc.base.Filter
import lucuma.itc.base.FixedOptics
import lucuma.itc.base.ITCConstants
import lucuma.itc.operation.ImageQualityCalculatable
import lucuma.itc.operation.DetectorsTransmissionVisitor
import lucuma.itc.shared.GmosParameters
import lucuma.itc.shared.ObservationDetails
import lucuma.itc.shared.ItcWarning
import lucuma.itc.shared.IfuMethod
import lucuma.itc.shared.IfuSingle
import lucuma.itc.shared.IfuRadial
import lucuma.itc.shared.IfuSum
import lucuma.itc.shared.Imaging
import lucuma.itc.shared.Spectroscopy
import lucuma.core.enum
import edu.gemini.spModel.gemini.gmos.GmosCommonType
import edu.gemini.spModel.gemini.gmos.GmosNorthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import java.util

/**
  * Gmos specification class
  */
object Gmos { //Plate scales for original and Hamamatsu CCD's (temporary)
  val ORIG_PLATE_SCALE = 0.0727
  val HAM_PLATE_SCALE = 0.080778
  /**
    * Related files will be in this subdir of lib
    */
  val INSTR_DIR = "gmos"
  // Instrument reads its configuration from here.
  private val AD_SATURATION = 65535
  val E2V_DETECTOR_PIXELS = 6218 // same for both sites

  def legacySite(site: enum.Site) = site match {
    case enum.Site.GN => edu.gemini.spModel.core.Site.GN
    case enum.Site.GS => edu.gemini.spModel.core.Site.GS
  }
}

abstract class Gmos[D](val gp: GmosParameters[D], val odp: ObservationDetails, val FILENAME: String, val detectorCcdIndex: Int) extends Instrument(Gmos.legacySite(gp.site), Instrument.Bands.VISIBLE, Gmos.INSTR_DIR, FILENAME) with BinningProvider with SpectroscopyInstrument {
  val _detectorCcdIndex = detectorCcdIndex
  val _sampling = super.getSampling
  val _Filter: Option[Filter]
  val _gratingOptics: Option[GmosGratingOptics]
  // TODO: filter is not yet defined, need to work with filter from gp, clean this up
  // System.out.println(gp.filter.getClass.toString)
  // RESTORE
  // if (!gp.filter().equals(GmosNorthType.FilterNorth.NONE) && !gp.filter().equals(GmosSouthType.FilterSouth.NONE)) {
  // UPGRADE
  // }
  val _fixedOptics = new FixedOptics(getDirectory + "/", getPrefix)
  addComponent(_fixedOptics)
  //Choose correct CCD QE curve
  // gp.ccdType match { // E2V, site dependent
  //   case E2V =>
  //     gp.legacySite match { // E2V for GN: gmos_n_E2V4290DDmulti3.dat      => EEV DD array
  //       case GN =>
  //         _detector = new Detector(getDirectory + "/", getPrefix, "E2V4290DDmulti3", "EEV DD array")
  //
  //       // E2V for GS: gmos_n_cdd_red.dat              => EEV legacy
  //       case GS =>
  //         _detector = new Detector(getDirectory + "/", getPrefix, "ccd_red", "EEV legacy array")
  //       case _ =>
  //         throw new Error("invalid site")
  //     }
  //     _detector.setDetectorPixels(detectorPixels)
  //     if (detectorCcdIndex == 0) _instruments = Array[Gmos](this)
  //
  //   // Hamamatsu, both sites: gmos_n_CCD-{R,G,B}.dat        =>  Hamamatsu (R,G,B)
  //   case HAMAMATSU =>
  //     val fileName = getCcdFiles(detectorCcdIndex)
  //     val name = getCcdNames(detectorCcdIndex)
  //     _detector = new Detector(getDirectory + "/", getPrefix, fileName, "Hamamatsu array", name)
  //     _detector.setDetectorPixels(detectorPixels)
  //     if (detectorCcdIndex == 0) _instruments = createCcdArray
  //   case _ =>
  //     throw new Error("invalid ccd type")
  // }
//   if (isIfuUsed && getIfuMethod.isDefined) {
//     if (getIfuMethod.get.isInstanceOf[IfuSingle]) _IFU = new IFUComponent(getPrefix, getIfuMethod.get.asInstanceOf[IfuSingle].offset)
//     else if (getIfuMethod.get.isInstanceOf[IfuRadial]) {
//       val ifu = getIfuMethod.get.asInstanceOf[IfuRadial]
//       _IFU = new IFUComponent(getPrefix, ifu.minOffset, ifu.maxOffset)
//     }
//     else if (getIfuMethod.get.isInstanceOf[IfuSum]) {
//       val num = odp.analysisMethod.asInstanceOf[IfuSum].num
//       _IFU = new IFUComponent(getPrefix, num, isIfu2)
//     }
//     else throw new Error("invalid IFU type")
//     addComponent(_IFU)
//   }
//   // TODO: grating is not yet defined, need to work with grating from gp, clean this up
//   if (!(gp.legacyGrating == GmosNorthType.DisperserNorth.MIRROR) && !(gp.grating == GmosSouthType.DisperserSouth.MIRROR)) {
//     _gratingOptics = new GmosGratingOptics(getDirectory + "/" + getPrefix, gp.legacyGrating, _detector, gp.centralWavelength.nanometer.toDouble, _detector.getDetectorPixels, gp.spectralBinning)
//     _sampling = _gratingOptics.dispersion
//     addDisperser(_gratingOptics)
//     // we only need the detector transmission visitor for the spectroscopy case (i.e. if there is a grating)
//     if (detectorCcdIndex == 0) {
//       val nmppx = _gratingOptics.dispersion
//       gp.legacyCcdType match {
//         case E2V =>
//           _dtv = new DetectorsTransmissionVisitor(gp, nmppx, getDirectory + "/" + getPrefix + "ccdpix" + Instrument.getSuffix)
//         case HAMAMATSU =>
//           _dtv = new DetectorsTransmissionVisitor(gp, nmppx, getDirectory + "/" + getPrefix + "ccdpix_hamamatsu" + Instrument.getSuffix)
//         case _ =>
//           throw new Error("invalid ccd type")
//       }
//     }
//   }
//   addComponent(_detector)
//   gmosSaturLimitWarning = new GmosSaturLimitRule(getADSaturation, wellDepth, getSpatialBinning, getSpectralBinning, gain, 0.95)
//   // validate the current configuration
//   validate()
//   protected var _dtv = null
//   // Used as a desperate solution when multiple detectors need to be handled differently (See REL-478).
//   // For EEV holds the one instance one the Gmos instrument, for Hamamatsu, contains 3 one Gmos instance for
//   // each of the three detectors.
//   protected var _instruments = null
//   // Keep a reference to the color filter to ask for effective wavelength
//   protected var _Filter = null
//   protected var _IFU = null
//   protected var _gratingOptics = null
//   protected var _detector = null
//   protected var _sampling = .0
//   private var _detectorCcdIndex = 0 // 0, 1, or 2 when there are multiple CCDs in the detector
//   private var gmosSaturLimitWarning = null // GMOS-specific saturation limit warning
//
//   def detectorPixels
//
//   /** {@inheritDoc } */
  override def getSlitWidth = gp.slitWidth()
    // if (gp.legacyFpMask.isIFU) 0.3
//   else if (gp.customSlitWidth.isDefined) gp.customSlitWidth.get.width.toDoubleDegrees / 3600
//   else gp.fpMask.slitWidth.get.toDoubleDegrees / 3600
//
//   /**
//     * Returns an array containing this instrument, or, if there are multiple detector CCDs,
//     * an array containing instances of this instrument with the CCD set differently
//     * (Used to implement hamamatsu CCD support).
//     */
//   def getDetectorCcdInstruments = _instruments
//
//   /**
//     * Index of current CCD in detector
//     *
//     * @return 0, 1, or 2 when there are multiple CCDs in the detector (default: 0)
//     */
//   def getDetectorCcdIndex = _detectorCcdIndex
//
//   /**
//     * Returns the name of the detector CCD
//     */
//   def getDetectorCcdName = _detector.getName
//
  /**
    * Returns the effective observing wavelength.
    * This is properly calculated as a flux-weighted average of
    * observed spectrum.  So this may be temporary.
    *
    * @return Effective wavelength in nm
    */
  override def getEffectiveWavelength =
    (_gratingOptics, _Filter) match {
      case (Some(g), None) => g.getEffectiveWavelength.toInt
      case (None, Some(f)) => f.getEffectiveWavelength.toInt
      case _ => ???
    }
  //   if (disperser.isEmpty) _Filter.getEffectiveWavelength.toInt
  // else _gratingOptics.getEffectiveWavelength.toInt
//
//   def getGrating = gp.legacyGrating
//
//   def getGratingDispersion = _gratingOptics.dispersion
//
//   /**
//     * Returns the subdirectory where this instrument's data files are.
//     */
  override def getDirectory = ITCConstants.LIB + "/" + Gmos.INSTR_DIR

//   override def getPixelSize = gp.legacyCcdType match {
//     case E2V =>
//       Gmos.ORIG_PLATE_SCALE * gp.spatialBinning
//     case HAMAMATSU =>
//       Gmos.HAM_PLATE_SCALE * gp.spatialBinning
//     case _ =>
//       throw new Error("invalid ccd type")
//   }
//
//   def getSpectralPixelWidth = _gratingOptics.getPixelWidth
//
//   override def getSampling = _sampling
//
  override def getSpectralBinning = gp.spectralBinning

  override def getSpatialBinning = gp.spatialBinning

//   def getADSaturation = Gmos.AD_SATURATION
//
//   def getIFU = _IFU
//
//   def isIfuUsed = gp.legacyFpMask.isIFU
//
//   def getIfuMethod = if (odp.analysisMethod.isInstanceOf[IfuMethod]) Option.apply(odp.analysisMethod.asInstanceOf[IfuMethod])
//   else Option.empty
//
//   def getFpMask = gp.legacyFpMask
//
//   def getCentralWavelength = gp.centralWavelength.nanometer.toDouble
//
//   def getObservingStart(shift: Double) = if (shift != 0) _gratingOptics.getStart(shift)
//   else getObservingStart
//
//   def getObservingEnd(shift: Double) = if (shift != 0) _gratingOptics.getEnd(shift)
//   else getObservingEnd
//
//   //Abstract class for Detector Pixel Transmission  (i.e.  Create Detector gaps)
//   def getDetectorTransmision = _dtv
//
//   def getGmosSaturLimitWarning = gmosSaturLimitWarning
//
  def isIfu2: Boolean
//
//   protected def createCcdArray
//
  protected def getPrefix: String
//
//   protected def getCcdFiles
//
//   protected def getCcdNames
//
//   private def validate() = { //Test to see that all conditions for Spectroscopy are met
//     if (odp.calculationMethod.isInstanceOf[Spectroscopy]) {
//       if (disperser.isEmpty) throw new RuntimeException("Spectroscopy calculation method is selected but a grating" + " is not.\nPlease select a grating and a " + "focal plane mask in the Instrument " + "configuration section.")
//       if (gp.fpMask == GmosNorthType.FPUnitNorth.FPU_NONE || gp.fpMask == GmosSouthType.FPUnitSouth.FPU_NONE) throw new RuntimeException("Spectroscopy calculation method is selected but a focal" + " plane mask is not.\nPlease select a " + "grating and a " + "focal plane mask in the Instrument " + "configuration section.")
//       if (gp.legacyFpMask == GmosNorthType.FPUnitNorth.CUSTOM_MASK || gp.fpMask == GmosSouthType.FPUnitSouth.CUSTOM_MASK) {
//         if (gp.customSlitWidth.isEmpty) throw new RuntimeException("Custom mask is selected but custom slit width is undefined.")
//         if (gp.customSlitWidth.get == GmosCommonType.CustomSlitWidth.OTHER) throw new RuntimeException("Slit width for the custom mask is not known.")
//       }
//       if ((gp.legacyFpMask.isIFU || isIfu2) && gp.spatialBinning != 1) throw new RuntimeException("Spatial binning must be 1 with IFU observations.\n" + "The GMOS fiber traces on the detector blend together if the detector is binned spatially\n" + "and the fibers cannot be extracted reliably using the Gemini IRAF data reduction package.")
//       // central wavelength, site dependent
//       val _centralWavelength = getCentralWavelength
//       gp.legacySite match { // User-input central wavelength for GN
//         case GN =>
//           if (_centralWavelength < 360 || _centralWavelength > 1000) throw new RuntimeException("Central wavelength must be between 360 nm and 1000 nm.")
//
//         // User-input central wavelength for GS
//         case GS =>
//           if (_centralWavelength < 300 || _centralWavelength > 1000) throw new RuntimeException("Central wavelength must be between 300 nm and 1000 nm.")
//         case _ =>
//           throw new RuntimeException("invalid site")
//       }
//     }
//     if (odp.calculationMethod.isInstanceOf[Imaging]) {
//       if (filter.isEmpty) throw new RuntimeException("Imaging calculation method is selected but a filter is not.")
//       if (disperser.isDefined) throw new RuntimeException("Imaging calculation method is selected but a grating" + " is also selected.\nPlease deselect the " + "grating or change the method to spectroscopy.")
//       if (!(gp.fpMask == GmosNorthType.FPUnitNorth.FPU_NONE) && !(gp.fpMask == GmosSouthType.FPUnitSouth.FPU_NONE)) throw new RuntimeException("Imaging calculation method is selected but a Focal" + " Plane Mask is also selected.\nPlease deselect the Focal Plane Mask" + " or change the method to spectroscopy.")
//       if (gp.customSlitWidth.isDefined) throw new RuntimeException("Imaging calculation method is selected but a Custom" + " Slit Width is also selected.\n")
//     }
//     if (isIfuUsed && getIfuMethod.isEmpty) throw new RuntimeException("IFU is selected but no IFU analysis method is selected.\nPlease deselect the IFU or" + " select an IFU analysis method.")
//     if (!isIfuUsed && getIfuMethod.isDefined) throw new RuntimeException("An IFU analysis method is selected but no IFU is selected.\nPlease select the IFU or" + " select another analysis method.")
//     // TODO: Implement once GMOS can be used with Altair
//     //        if (gp.altair().isDefined()) {
//     //            if (gp.altair().get().guideStarSeparation() < 0 || gp.altair().get().guideStarSeparation() > 45)
//     //                throw new RuntimeException("Altair Guide star distance must be between 0 and 45 arcsecs for GMOS.\n");
//     //        }
//   }
//
//   override def spectroscopyWarnings(r: SpectroscopyResult) = new util.ArrayList[ItcWarning]() {}
//
//   override def warnings = new util.ArrayList[WarningRule]() {}
}
