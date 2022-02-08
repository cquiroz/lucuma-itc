// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._
import io.circe.ACursor
import io.circe.Decoder
import io.circe.HCursor

final case class ItcResult(ccds: NonEmptyList[ItcCcd]) {

  // We may not need these
  def maxPeakPixelFlux: Int      = ccds.map(_.peakPixelFlux).maximum.toInt
  def maxAdu: Int                = ccds.map(_.adu).maximum
  def maxPercentFullWell: Double = ccds.map(_.percentFullWell).maximum
  def maxWellDepth: Double       = ccds.map(_.wellDepth).maximum
  def maxSingleSNRatio: Double   = ccds.map(_.singleSNRatio).maximum
  def maxTotalSNRatio: Double    = ccds.map(_.totalSNRatio).maximum

}

object ItcResult {

  // An "orElse" semigroup for ACursor
  private implicit val decoderSemigroup: Semigroup[ACursor] =
    new Semigroup[ACursor] {
      def combine(a: ACursor, b: ACursor): ACursor =
        if (a.failed) b else a
    }

  implicit val decoder: Decoder[ItcResult] =
    new Decoder[ItcResult] {
      def apply(c: HCursor): Decoder.Result[ItcResult] =
        (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
          .downField("ccds")
          .as[NonEmptyList[ItcCcd]]
          .map(ItcResult(_))

    }

}
