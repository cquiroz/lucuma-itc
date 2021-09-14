package lucuma.itc.gmos

import cats.syntax.all._
import lucuma.itc.shared.{GmosSouthParameters, ObservationDetails}
import lucuma.itc.base.Instrument
import lucuma.itc.base.Detector
import lucuma.core.enum.GmosSouthDetector
import lucuma.core.enum.GmosAmpGain
import lucuma.core.enum.GmosAmpReadMode
import lucuma.core.enum.GmosSouthFpu

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

final class GmosSouth(gsp: GmosSouthParameters, odp1: ObservationDetails, val detectorCcdIndex: Int) extends Gmos(gsp, odp1, GmosSouth.FILENAME, detectorCcdIndex) {
  //Choose correct CCD QE curve
  val (_detector, _instruments) = gsp.ccdType match { // E2V, site dependent
    case GmosSouthDetector.E2V =>
      val _detector = new Detector(getDirectory + "/", getPrefix, "ccd_red", "EEV legacy array")
      _detector.setDetectorPixels(detectorPixels)
      (_detector, if (detectorCcdIndex == 0) Array[Gmos[GmosSouthDetector]](this) else Array())

    // Hamamatsu, both sites: gmos_n_CCD-{R,G,B}.dat        =>  Hamamatsu (R,G,B)
    case GmosSouthDetector.Hamamatsu =>
      val fileName = getCcdFiles(detectorCcdIndex)
      val name = getCcdNames(detectorCcdIndex)
      val _detector = new Detector(getDirectory + "/", getPrefix, fileName, "Hamamatsu array", name)
      _detector.setDetectorPixels(detectorPixels)
      (_detector, if (detectorCcdIndex == 0) createCcdArray else Array())
  }


  override def isIfu2 = gsp.fpMask === GmosSouthFpu.Ifu2Slits

  override protected def createCcdArray = Array[Gmos[GmosSouthDetector]](this, new GmosSouth(gsp, odp, 1), new GmosSouth(gsp, odp, 2))

  override protected def getPrefix = GmosSouth.INSTR_PREFIX

  override protected def getCcdFiles = GmosSouth.DETECTOR_CCD_FILES

  override protected def getCcdNames = GmosSouth.DETECTOR_CCD_NAMES

  override def wellDepth: Double = gsp.ccdType match {
    case GmosSouthDetector.E2V =>
      GmosSouth.WellDepth
    case GmosSouthDetector.Hamamatsu =>
      GmosSouth.HAMAMATSU_WELL_DEPTH
  }

  override def detectorPixels: Int = gsp.ccdType match {
    case GmosSouthDetector.E2V =>
      Gmos.E2V_DETECTOR_PIXELS
    case GmosSouthDetector.Hamamatsu =>
      GmosSouth.HAMAMATSU_DETECTOR_PIXELS
  }

  override def gain = getMeanGain(gp.ampGain, gp.ampReadMode, gsp.ccdType)

  /**
    * Calculates the mean gain for the given parameters for GMOS.
    */
  def getMeanGain(gain: GmosAmpGain, readMode: GmosAmpReadMode, detectorManufacturer: GmosSouthDetector): Double =
    (detectorManufacturer, readMode, gain) match {
      case (GmosSouthDetector.E2V, GmosAmpReadMode.Fast, GmosAmpGain.High) => 5.0
      case (GmosSouthDetector.E2V, GmosAmpReadMode.Fast, GmosAmpGain.Low) => 2.5
      case (GmosSouthDetector.E2V, GmosAmpReadMode.Slow, GmosAmpGain.High) => 4.4
      case (GmosSouthDetector.E2V, GmosAmpReadMode.Slow, GmosAmpGain.Low) => 2.2
      case (GmosSouthDetector.Hamamatsu, GmosAmpReadMode.Fast, GmosAmpGain.High) => 5.2
      case (GmosSouthDetector.Hamamatsu, GmosAmpReadMode.Fast, GmosAmpGain.Low) => 1.6
      case (GmosSouthDetector.Hamamatsu, GmosAmpReadMode.Slow, GmosAmpGain.High) => 4.4
      case (GmosSouthDetector.Hamamatsu, GmosAmpReadMode.Slow, GmosAmpGain.Low) => 1.8
    }

}
