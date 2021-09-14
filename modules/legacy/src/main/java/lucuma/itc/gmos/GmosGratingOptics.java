package lucuma.itc.gmos;

import lucuma.itc.base.Detector;
import lucuma.itc.base.GratingOptics;
import edu.gemini.spModel.gemini.gmos.GmosCommonType;

/**
 * This represents the transmission and properties of the Grating optics.
 */
public final class GmosGratingOptics extends GratingOptics {

    public GmosGratingOptics(final String directory,
                             final String gratingName,
                             final Detector detector,
                             final double centralWavelength,
                             final int detectorPixels,
                             final int spectralBinning) {

        super(directory, gratingName, gratingsName(detector), centralWavelength, detectorPixels, spectralBinning);
    }

    private static String gratingsName(final Detector detector) {
        return detector.toString().contains("EEV") ? "eev_gratings" : "gratings";
    }

    // IFU-2 case
    public double getStart(double shift) {
        return centralWavelength - (data.apply(gratingName).dispersion() * detectorPixels / 2) + shift;
    }

    // IFU-2 case
    public double getEnd(double shift) {
        return centralWavelength + (data.apply(gratingName).dispersion() * detectorPixels / 2) + shift;
    }

}
