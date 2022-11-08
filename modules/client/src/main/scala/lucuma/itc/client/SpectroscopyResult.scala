// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.traverse.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor

final case class SpectroscopyResult(
  serverVersion: String,
  dataVersion:   Option[String],
  result:        Option[ItcResult]
)

object SpectroscopyResult {

  given Decoder[SpectroscopyResult] with
    def apply(c: HCursor): Decoder.Result[SpectroscopyResult] =
      for {
        s <- c.downField("serverVersion").as[String]
        d <- c.downField("dataVersion").as[Option[String]]
        r <- c.downField("results").downArray.downField("itc").success.traverse(_.as[ItcResult])
      } yield SpectroscopyResult(s, d, r)

  given Eq[SpectroscopyResult] with
    def eqv(x: SpectroscopyResult, y: SpectroscopyResult): Boolean =
      x.serverVersion === y.serverVersion &&
        x.dataVersion === y.dataVersion &&
        x.result === y.result

}
