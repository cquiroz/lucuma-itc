// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor

final class ImageQualityOps(val self: ImageQuality) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case ImageQuality.PointOne     => "PERCENT_20"
      case ImageQuality.PointTwo     => "PERCENT_20"
      case ImageQuality.PointThree   => "PERCENT_20"
      case ImageQuality.PointFour    => "PERCENT_20"
      case ImageQuality.PointSix     => "PERCENT_70"
      case ImageQuality.PointEight   => "PERCENT_70"
      case ImageQuality.OnePointZero => "PERCENT_85"
      case ImageQuality.OnePointFive => "ANY"
      case ImageQuality.TwoPointZero => "ANY"
    }
}

trait ToImageQualityOps {
  implicit def toImageQualityOps(self: ImageQuality): ImageQualityOps =
    new ImageQualityOps(self)
}

final class CloudExtinctionOps(val self: CloudExtinction) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case CloudExtinction.PointOne       => "PERCENT_20"
      case CloudExtinction.PointThree     => "PERCENT_20"
      case CloudExtinction.PointFive      => "PERCENT_50"
      case CloudExtinction.OnePointZero   => "PERCENT_70"
      case CloudExtinction.OnePointFive   => "PERCENT_80"
      case CloudExtinction.TwoPointZero   => "PERCENT_90"
      case CloudExtinction.ThreePointZero => "ANY"
    }
}

trait ToCloudExtinctionOps {
  implicit def toCloudExtinctionOps(self: CloudExtinction): CloudExtinctionOps =
    new CloudExtinctionOps(self)
}

final class WaterVaporOps(val self: WaterVapor) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case WaterVapor.VeryDry => "PERCENT_20"
      case WaterVapor.Dry     => "PERCENT_50"
      case WaterVapor.Median  => "PERCENT_80"
      case WaterVapor.Wet     => "ANY"
    }
}

trait ToWaterVaporOps {
  implicit def toWaterVaporOps(self: WaterVapor): WaterVaporOps =
    new WaterVaporOps(self)
}

final class SkyBackgroundOps(val self: SkyBackground) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case SkyBackground.Darkest => "PERCENT_20"
      case SkyBackground.Dark    => "PERCENT_50"
      case SkyBackground.Gray    => "PERCENT_80"
      case SkyBackground.Bright  => "ANY"
    }
}

trait ToSkyBackgroundOps {
  implicit def toSkyBackgroundOps(self: SkyBackground): SkyBackgroundOps =
    new SkyBackgroundOps(self)
}

trait ToConditionsOps
    extends ToImageQualityOps
    with ToCloudExtinctionOps
    with ToWaterVaporOps
    with ToSkyBackgroundOps

object conditions extends ToConditionsOps
