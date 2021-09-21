// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.model.syntax

import lucuma.core.enum.GmosSouthFpu
import lucuma.core.enum.GmosNorthFpu

final class DisperserGsOps(val self: GmosSouthFpu) extends AnyVal {
  def isIFU: Boolean = self match {
    case GmosSouthFpu.Ifu2Slits | GmosSouthFpu.IfuBlue | GmosSouthFpu.IfuRed | GmosSouthFpu.IfuNS2Slits | GmosSouthFpu.IfuNSBlue| GmosSouthFpu.IfuNSRed => true
    case _ => false
  }
}

final class DisperserGnOps(val self: GmosNorthFpu) extends AnyVal {
  def isIFU: Boolean = self match {
    case GmosNorthFpu.Ifu2Slits | GmosNorthFpu.IfuBlue | GmosNorthFpu.IfuRed => true
    case _ => false
  }
}

trait ToDisperserOps {
  implicit def toDisperserGsOps(self: GmosSouthFpu): DisperserGsOps =
    new DisperserGsOps(self)

  implicit def toDisperserGnOps(self: GmosNorthFpu): DisperserGnOps =
    new DisperserGnOps(self)
}

object disperser extends ToDisperserOps 
