package lucuma.itc.gmos

import edu.gemini.spModel.gemini.gmos.{GmosSouthType, InstGmosSouth}
import lucuma.itc.shared.{GmosParameters, ObservationDetails}
import lucuma.itc.base.Instrument
import lucuma.core.enum.GmosDetector
import lucuma.core.enum.GmosAmpGain
import edu.gemini.spModel.gemini.gmos.GmosCommonType
import lucuma.core.enum.GmosAmpReadMode

/**
  * Gmos specification class
  */
object GmosSouth { // value taken from instrument's web documentation
  val WellDepth: Double = 106000
  // Average full well depth of 12 amplifiers for GMOS-N Hamamatsu CCD
  val HAMAMATSU_WELL_DEPTH: Double = 150000
  val HAMAMATSU_DETECTOR_PIXELS: Int = 6266

  /**
    * Related files will start with this prefix
    */
  val INSTR_PREFIX = "gmos_s_"
  // Instrument reads its configuration from here.
  val FILENAME = "gmos_s" + Instrument.getSuffix
  //
  // Detector data files (see REL-478)
  val DETECTOR_CCD_FILES = Array("ccd_hamamatsu_bb", "ccd_hamamatsu_hsc", "ccd_hamamatsu_bb")
  // Detector display names corresponding to the detectorCcdIndex
  val DETECTOR_CCD_NAMES = Array("BB", "HSC", "SC");
}

final class GmosSouth(gp1: GmosParameters, odp1: ObservationDetails, val detectorCcdIndex: Int) extends Gmos(gp1, odp1, GmosSouth.FILENAME, detectorCcdIndex) {

  override def isIfu2 = getFpMask eq GmosSouthType.FPUnitSouth.IFU_1

  override protected def createCcdArray = Array[Gmos](this, new GmosSouth(gp, odp, 1), new GmosSouth(gp, odp, 2))

  override protected def getPrefix = GmosSouth.INSTR_PREFIX

  override protected def getCcdFiles = GmosSouth.DETECTOR_CCD_FILES

  override protected def getCcdNames = GmosSouth.DETECTOR_CCD_NAMES

  override def wellDepth: Double = gp.ccdType match {
    case GmosDetector.E2V =>
      GmosSouth.WellDepth
    case GmosDetector.HAMAMATSU =>
      GmosSouth.HAMAMATSU_WELL_DEPTH
  }

  override def detectorPixels: Int = gp.ccdType match {
    case GmosDetector.E2V =>
      Gmos.E2V_DETECTOR_PIXELS
    case GmosDetector.HAMAMATSU =>
      GmosSouth.HAMAMATSU_DETECTOR_PIXELS
  }

  // UPGRADE
  private val ampGain = gp.ampGain match {
    case GmosAmpGain.High => GmosCommonType.AmpGain.HIGH
    case GmosAmpGain.Low => GmosCommonType.AmpGain.LOW
  }

  private val ampReadMode = gp.ampReadMode match {
    case GmosAmpReadMode.Fast => GmosCommonType.AmpReadMode.FAST
    case GmosAmpReadMode.Slow => GmosCommonType.AmpReadMode.SLOW
  }

  override def gain = InstGmosSouth.getMeanGain(ampGain, ampReadMode, gp.legacyCcdType)
}
