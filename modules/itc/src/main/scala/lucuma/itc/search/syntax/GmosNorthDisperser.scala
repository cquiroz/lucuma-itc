// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import eu.timepit.refined.auto._
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosNorthDisperser._
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.search.Coverage
import spire.math.Rational

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
final class GmosNorthDisperserOps(val self: GmosNorthDisperser) extends AnyVal {

  /**
   * Reference wavelength (nm) and resolution for 0.5" slit.
   * @see
   *   http://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings
   */
  private def reference: (Long, Long) =
    self match {
      case B1200_G5301 => (463, 3744)
      case R831_G5302  => (757, 4396)
      case B600_G5303  => (461, 1688) // obsolete
      case B600_G5307  => (461, 1688)
      case R600_G5304  => (926, 3744)
      case B480_G5309  => (422, 1520)
      case R400_G5305  => (764, 1918)
      case R150_G5306  => (717, 631)  // obsolete
      case R150_G5308  => (717, 631)
    }

  /**
   * Δλ for 0.5" slit.
   * @see
   *   http://hyperphysics.phy-astr.gsu.edu/hbase/phyopt/gratres.html
   */
  private def Δλ: Rational = {
    val (λ, r) = reference
    Rational(λ, r) // r = λ / Δλ
  }

  /** Resolution at λ with the specified slit width (arcsec). */
  def resolution(λ: Wavelength, slitWidth: Angle): Rational =
    (λ.nanometer.value / Δλ) * (Rational(1, 2) / Angle.signedDecimalArcseconds.get(slitWidth))

  /**
   * Simultaneous coverage with Hamamatsu detectors.
   * @see
   *   http://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings
   */
  def simultaneousCoverage: Wavelength =
    self match {
      case B1200_G5301 => Wavelength.fromNanometers(164).get
      case R831_G5302  => Wavelength.fromNanometers(235).get
      case B600_G5303  => Wavelength.fromNanometers(276).get // obsolete, value with old e2v detector
      case B600_G5307  => Wavelength.fromNanometers(317).get
      case R600_G5304  => Wavelength.fromNanometers(328).get
      case R400_G5305  => Wavelength.fromNanometers(472).get
      case B480_G5309  => Wavelength(390000)
      case R150_G5306  =>
        Wavelength.fromNanometers(1071).get // obsolete, value with old e2v detector
      case R150_G5308  => Wavelength.fromNanometers(1219).get
    }

  /** Compute the coverage of this disperser, given a central wavelength. */
  def coverage(λ: Wavelength): Coverage =
    Coverage.centered(λ, simultaneousCoverage)

  /**
   * Dispersion (pm/pixel) with Hamamatsu detectors.
   * @see
   *   http://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings
   */
  def dispersion: Int =
    self match {
      case B1200_G5301 => 26
      case R831_G5302  => 38
      case B600_G5303  => 45  // obsolete, value with old e2v detector
      case B600_G5307  => 50
      case R600_G5304  => 52
      case R400_G5305  => 74
      case B480_G5309  => 62
      case R150_G5306  => 174 // obsolete, value with old e2v detector
      case R150_G5308  => 193
    }

  // pedantic: tags are the same in OCS2 and OCS3 but this is just a coincidence
  def ocs2Tag: String =
    self match {
      case B1200_G5301 => "B1200_G5301"
      case R831_G5302  => "R831_G5302"
      case B480_G5309  => "B480_G5309"
      case B600_G5303  => "B600_G5303"
      case B600_G5307  => "B600_G5307"
      case R600_G5304  => "R600_G5304"
      case R400_G5305  => "R400_G5305"
      case R150_G5306  => "R150_G5306"
      case R150_G5308  => "R150_G5308"
    }
}

trait ToGmosNorthDisperserOps {
  implicit def toGmosNorthDisperserOps(self: GmosNorthDisperser): GmosNorthDisperserOps =
    new GmosNorthDisperserOps(self)
}

object gmosnorthdisperser extends ToGmosNorthDisperserOps
