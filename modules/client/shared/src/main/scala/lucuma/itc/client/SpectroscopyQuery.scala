// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.traverse.*
import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject

object SpectroscopyQuery extends GraphQLOperation[Unit] {

  type Data      = SpectroscopyResult
  type Variables = SpectroscopyIntegrationTimeInput

  override val document: String =
    """
      query Spectroscopy($spec: SpectroscopyIntegrationTimeInput!) {
        spectroscopyIntegrationTime(input: $spec) {
          serverVersion
          dataVersion
          result {
            exposures
            exposureTime {
              microseconds
            }
            signalToNoise
          }
        }
      }
    """

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[SpectroscopyIntegrationTimeInput] { input =>
      JsonObject(
        "spec" -> Encoder[SpectroscopyIntegrationTimeInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[SpectroscopyResult] =
    (c: HCursor) => c.downField("spectroscopyIntegrationTime").as[SpectroscopyResult]

}
