package lucuma.itc.operation

import lucuma.itc.base.{ZeroMagnitudeStar, SampledSpectrum, SampledSpectrumVisitor}
// import edu.gemini.spModel.core.{SurfaceBrightness, MagnitudeSystem, BrightnessUnit, MagnitudeBand}
import lucuma.core.enum.MagnitudeSystem
import lucuma.core.enum.MagnitudeBand

/**
 * The NormalizeVisitor class is used to perform Normalization to the SED.
 * Normalization rescales the SED so that the average flux in a
 * specified waveband is equal to a specified value.
 * This is where unit conversion happens.
 */
final class NormalizeVisitor(band: MagnitudeBand, userNorm: Double, units: MagnitudeSystem) extends SampledSpectrumVisitor {

  /**
   * Implements the visitor interface.
   * Performs the normalization.
   */
  def visit(sed: SampledSpectrum): Unit = {

    val norm = units match {

      case MagnitudeSystem.Vega =>//| SurfaceBrightness.Vega =>
        val zeropoint = ZeroMagnitudeStar.getAverageFlux(band)
        zeropoint * Math.pow(10.0, -0.4 * userNorm)

      case MagnitudeSystem.AB =>//| SurfaceBrightness.AB =>
        5.632e10 * Math.pow(10, -0.4 * userNorm) / band.center.nanometer.value.toDouble

      case MagnitudeSystem.Jy =>//| SurfaceBrightness.Jy =>
        userNorm * 1.509e7 / band.center.nanometer.value.toDouble

      case MagnitudeSystem.Watts =>//| SurfaceBrightness.Watts =>
        userNorm * band.center.nanometer.value.toDouble / 1.988e-13

      case MagnitudeSystem.ErgsWavelength =>//| SurfaceBrightness.ErgsWavelength =>
        userNorm * band.center.nanometer.value.toDouble / 1.988e-14

      case MagnitudeSystem.ErgsFrequency =>//| SurfaceBrightness.ErgsFrequency =>
        userNorm * 1.509e30 / band.center.nanometer.value.toDouble

    }

    // Calculate avg flux density in chosen normalization band.
    val average = sed.getAverage(band.start.nanometer.value.toDouble, band.end.nanometer.value.toDouble)

    // Calculate multiplier.
    val multiplier = norm / average

    // Apply normalization, multiply every value in the SED by
    // multiplier and then its average in specified band will be
    // the required amount.
    sed.rescaleY(multiplier)
  }

}
