// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import cats.implicits._
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthFilter._
import lucuma.core.math.Wavelength
import lucuma.itc.search.Coverage

final class GmosNorthFilterOps(val self: GmosNorthFilter) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case GPrime           => "g_G0301"
      case RPrime           => "r_G0303"
      case IPrime           => "i_G0302"
      case ZPrime           => "z_G0304"
      case Z                => "Z_G0322"
      case Y                => "Y_G0323"
      case GG455            => "GG455_G0305"
      case OG515            => "OG515_G0306"
      case RG610            => "RG610_G0307"
      case CaT              => "CaT_G0309"
      case Ha               => "Ha_G0310"
      case HaC              => "HaC_G0311"
      case DS920            => "DS920_G0312"
      case SII              => "SII_G0317"
      case OIII             => "OIII_G0318"
      case OIIIC            => "OIIIC_G0319"
      case HeII             => "HeII_G0320"
      case HeIIC            => "HeIIC_G0321"
      case HartmannA_RPrime => "HartmannA_G0313_r_G0303"
      case HartmannB_RPrime => "HartmannB_G0314_r_G0303"
      case GPrime_GG455     => "g_G0301_GG455_G0305"
      case GPrime_OG515     => "g_G0301_OG515_G0306"
      case RPrime_RG610     => "r_G0303_RG610_G0307"
      case IPrime_CaT       => "i_G0302_CaT_G0309"
      case ZPrime_CaT       => "z_G0304_CaT_G0309"
      case UPrime           => "u_G0308"
    }

  // see http://www.gemini.edu/node/10621
  def coverage: Coverage = {
    import Wavelength.fromPicometers

    def cov(a: Int, b: Int = Int.MaxValue): Coverage =
      (fromPicometers.getOption(a), fromPicometers.getOption(b))
        .mapN(Coverage.apply)
        .getOrElse(sys.error("Invalid constant coverage."))

    self match {

      // Broad Band Imaging Filters
      case UPrime => cov(336000, 385000)
      case GPrime => cov(398000, 552000)
      case RPrime => cov(562000, 698000)
      case IPrime => cov(706000, 850000)
      case CaT    => cov(780000, 993000)
      case ZPrime => cov(848000)
      case Z      => cov(830000, 925000)
      case Y      => cov(970000, 1070000)
      // ri -- not in OCS3 … need to add

      // Narrow Band Imaging Filters
      case HeII  => cov(464000, 472000)
      case HeIIC => cov(474000, 482000)
      case OIII  => cov(496500, 501500)
      case OIIIC => cov(509000, 519000)
      case Ha    => cov(654000, 661000)
      case HaC   => cov(659000, 665000)
      case SII   => cov(669400, 673700)
      // OVIC? OVI? -- these aren't in OCS3 … need to add
      case DS920 => cov(912800, 931400)

      // Spectroscopy Blocking Filters
      case GG455 => cov(460000)
      case OG515 => cov(520000)
      case RG610 => cov(615000)

      // These are only used for engineering and will never be selected by search algorithm, but
      // we still need to handle them. For now we'll just pretend they have no coverage at all.
      case HartmannA_RPrime => Coverage.Empty
      case HartmannB_RPrime => Coverage.Empty

      // Allowed Filter Combinations
      case GPrime_GG455 => cov(460000, 552000)
      case GPrime_OG515 => cov(520000, 552000)
      case RPrime_RG610 => cov(615000, 698000)
      case IPrime_CaT   => cov(780000, 850000)
      case ZPrime_CaT   => cov(848000, 933000)
    }
  }

}

trait ToGmosNorthFilterOps {
  implicit def toGmosNorthFilterOps(self: GmosNorthFilter): GmosNorthFilterOps =
    new GmosNorthFilterOps(self)
}

final class GmosNorthFilterCompanionOps(val self: GmosNorthFilter.type) extends AnyVal {

  /** Filters to use for acquisition, sorted by wavelength. */
  def allAcquisition: List[GmosNorthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime).sortBy(_.wavelength)

}

trait ToGmosNorthFilterCompanionOps {
  implicit def toGmosNorthFilterCompanionOps(
    self: GmosNorthFilter.type
  ): GmosNorthFilterCompanionOps =
    new GmosNorthFilterCompanionOps(self)
}

object gmosnorthfilter extends ToGmosNorthFilterOps with ToGmosNorthFilterCompanionOps
