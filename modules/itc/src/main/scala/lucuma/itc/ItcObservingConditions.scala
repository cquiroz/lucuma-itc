// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import lucuma.core.enum._
import lucuma.itc.search.syntax.all._

final case class ItcObservingConditions(
  iq:      ImageQuality,
  cc:      CloudExtinction,
  wv:      WaterVapor,
  sb:      SkyBackground,
  airmass: Double
)

object ItcObservingConditions {
  def toItcAirmass(m: Double): Double =
    if (m <= 1.35) 1.2 else if (m <= 1.75) 1.5 else 2.0

  implicit val encoder: Encoder[ItcObservingConditions] =
    Encoder.forProduct5("iq", "cc", "wv", "sb", "airmass") { a =>
      (a.iq.ocs2Tag, a.cc.ocs2Tag, a.wv.ocs2Tag, a.sb.ocs2Tag, toItcAirmass(a.airmass))
    }

}
