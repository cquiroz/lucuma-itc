// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import io.circe.Json
import lucuma.core.math.Wavelength
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.syntax.all._

final case class ItcInstrumentDetails(mode: ObservingMode)

object ItcInstrumentDetails {

  def fromObservingMode(mode: ObservingMode): ItcInstrumentDetails =
    apply(mode)

  val encodeGmosNorthSpectroscopy: Encoder[ObservingMode.Spectroscopy.GmosNorth] =
    new Encoder[ObservingMode.Spectroscopy.GmosNorth] {
      def apply(a: ObservingMode.Spectroscopy.GmosNorth): Json =
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
          "fpMask"            -> Json.obj("FPUnitNorth" -> Json.fromString(a.fpu.ocs2Tag)),
          // Remaining fields are defaulted for now.
          "spectralBinning"   -> Json.fromInt(1),
          "site"              -> Json.fromString("GN"),
          "ccdType"           -> Json.fromString("HAMAMATSU"),
          "ampReadMode"       -> Json.fromString("SLOW"),
          "builtinROI"        -> Json.fromString("FULL_FRAME"),
          "spatialBinning"    -> Json.fromInt(1),
          "customSlitWidth"   -> Json.Null,
          "ampGain"           -> Json.fromString("LOW")
        )
    }

  implicit val encoder: Encoder[ItcInstrumentDetails] =
    new Encoder[ItcInstrumentDetails] {
      def apply(a: ItcInstrumentDetails): Json =
        a.mode match {
          case a: ObservingMode.Spectroscopy.GmosNorth =>
            Json.obj("GmosParameters" -> encodeGmosNorthSpectroscopy(a))
        }
    }

}
