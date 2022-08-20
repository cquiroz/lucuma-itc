// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthGrating.*
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthGrating.*

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
trait GmosSouthGratingSyntax:
  extension (self: GmosSouthGrating)
    // pedantic: tags are the same in OCS2 and OCS3 but this is just a coincidence
    def ocs2Tag: String =
      self match
        case B1200_G5321 => "B1200_G5321"
        case R831_G5322  => "R831_G5322"
        case B600_G5323  => "B600_G5323"
        case R600_G5324  => "R600_G5324"
        case B480_G5327  => "B480_G5327"
        case R400_G5325  => "R400_G5325"
        case R150_G5326  => "R150_G5326"

object gmossouthgrating extends GmosSouthGratingSyntax

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
trait GmosNorthGratingSyntax:
  extension (self: GmosNorthGrating)
    // pedantic: tags are the same in OCS2 and OCS3 but this is just a coincidence
    def ocs2Tag: String =
      self match
        case B1200_G5301 => "B1200_G5301"
        case R831_G5302  => "R831_G5302"
        case B480_G5309  => "B480_G5309"
        case B600_G5303  => "B600_G5303"
        case B600_G5307  => "B600_G5307"
        case R600_G5304  => "R600_G5304"
        case R400_G5305  => "R400_G5305"
        case R150_G5306  => "R150_G5306"
        case R150_G5308  => "R150_G5308"

object gmosnorthgrating extends GmosNorthGratingSyntax

trait GmosSouthFilterSyntax:
  import lucuma.core.enums.GmosSouthFilter
  import lucuma.core.enums.GmosSouthFilter.*
  extension (self: GmosSouthFilter)
    def ocs2Tag: String =
      self match
        case GPrime           => "g_G0301"
        case RPrime           => "r_G0303"
        case IPrime           => "i_G0302"
        case ZPrime           => "z_G0304"
        case Z                => "Z_G0322"
        case Y                => "Y_G0323"
        case GG455            => "GG455_G0305"
        case OG515            => "OG515_G0306"
        case RG610            => "RG610_G0307"
        case RG780            => "RG780_G0334"
        case CaT              => "CaT_G0309"
        case Ha               => "Ha_G0310"
        case HaC              => "HaC_G0311"
        case SII              => "SII_G0317"
        case OIII             => "OIII_G0318"
        case OIIIC            => "OIIIC_G0319"
        case HeII             => "HeII_G0320"
        case HeIIC            => "HeIIC_G0321"
        case Lya395           => "Lya395_G0342"
        case HartmannA_RPrime => "HartmannA_G0313_r_G0303"
        case HartmannB_RPrime => "HartmannB_G0314_r_G0303"
        case GPrime_GG455     => "g_G0301_GG455_G0305"
        case GPrime_OG515     => "g_G0301_OG515_G0306"
        case RPrime_RG610     => "r_G0303_RG610_G0307"
        case IPrime_RG780     => "i_G0327_RG780_G0334"
        case IPrime_CaT       => "i_G0302_CaT_G0309"
        case ZPrime_CaT       => "z_G0304_CaT_G0309"
        case UPrime           => "u_G0308"

object gmossouthfilter extends GmosSouthFilterSyntax

trait GmosNorthFilterSyntax:
  import lucuma.core.enums.GmosNorthFilter
  import lucuma.core.enums.GmosNorthFilter.*
  extension (self: GmosNorthFilter)
    def ocs2Tag: String =
      self match
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

object gmosnorthfilter extends GmosNorthFilterSyntax

trait GmosNorthFpuSyntax:
  import lucuma.core.enums.GmosNorthFpu
  import lucuma.core.enums.GmosNorthFpu.*
  extension (self: GmosNorthFpu)
    def ocs2Tag: String =
      self match
        case Ifu2Slits     => "IFU_1"
        case IfuBlue       => "IFU_2"
        case IfuRed        => "IFU_3"
        case Ns0           => "NS_0"
        case Ns1           => "NS_1"
        case Ns2           => "NS_2"
        case Ns3           => "NS_3"
        case Ns4           => "NS_4"
        case Ns5           => "NS_5"
        case LongSlit_0_25 => "LONGSLIT_1"
        case LongSlit_0_50 => "LONGSLIT_2"
        case LongSlit_0_75 => "LONGSLIT_3"
        case LongSlit_1_00 => "LONGSLIT_4"
        case LongSlit_1_50 => "LONGSLIT_5"
        case LongSlit_2_00 => "LONGSLIT_6"
        case LongSlit_5_00 => "LONGSLIT_7"

object gmosnorthfpu extends GmosNorthFpuSyntax

trait GmosSouthFpuSyntax:
  import lucuma.core.enums.GmosSouthFpu
  import lucuma.core.enums.GmosSouthFpu.*
  extension (self: GmosSouthFpu)
    def ocs2Tag: String =
      self match {
        case Ifu2Slits           => "IFU_1"
        case IfuNS2Slits         => "IFU_N"
        case IfuBlue | IfuNSBlue => "IFU_2"
        case IfuRed | IfuNSRed   => "IFU_3"
        case Ns1                 => "NS_1"
        case Ns2                 => "NS_2"
        case Ns3                 => "NS_3"
        case Ns4                 => "NS_4"
        case Ns5                 => "NS_5"
        case LongSlit_0_25       => "LONGSLIT_1"
        case LongSlit_0_50       => "LONGSLIT_2"
        case LongSlit_0_75       => "LONGSLIT_3"
        case LongSlit_1_00       => "LONGSLIT_4"
        case LongSlit_1_50       => "LONGSLIT_5"
        case LongSlit_2_00       => "LONGSLIT_6"
        case LongSlit_5_00       => "LONGSLIT_7"
        case Bhros               => "BHROs"
      }

object gmossouthfpu extends GmosSouthFpuSyntax
