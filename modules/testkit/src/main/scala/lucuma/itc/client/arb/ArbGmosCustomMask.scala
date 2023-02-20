// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbGmosCustomMask {

  import ArbEnumerated.*

  given Arbitrary[GmosCustomMask] =
    Arbitrary {
      for {
        w <- arbitrary[GmosCustomSlitWidth]
        n <- arbitrary[String]
      } yield GmosCustomMask(w, n)
    }

  given Cogen[GmosCustomMask] =
    Cogen[(
      GmosCustomSlitWidth,
      String
    )].contramap { a => (
      a.slitWidth,
      a.fileName
    )}

}

object ArbGmosCustomMask extends ArbGmosCustomMask
