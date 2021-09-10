package lucuma.itc.base

/**
  * Definition of capabilities needed for instruments that support spectroscopy.
  */
trait SpectroscopyInstrument {
  /** Gets the slit width of the mask in arcsec. */
  def getSlitWidth: Double
}
