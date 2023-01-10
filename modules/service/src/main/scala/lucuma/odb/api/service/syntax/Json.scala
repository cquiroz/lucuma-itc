// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service.syntax

import clue.model.StreamingMessage.FromServer
import clue.model.StreamingMessage.FromServer.Data
import clue.model.StreamingMessage.FromServer.DataWrapper
import clue.model.StreamingMessage.FromServer.Error
import io.circe.Json

trait JsonSyntax:
  extension (self: Json)

    def objectField(n: String): Option[Json] =
      self.hcursor.downField(n).focus

    def errorsField: Option[Json] =
      objectField("errors").filterNot(_.isNull)

    def dataField: Option[Json] =
      objectField("data").filterNot(_.isNull)

    private def unexpectedErrorMessage(id: String): FromServer =
      Error(
        id,
        Json.arr(
          Json.obj(
            "message" -> Json.fromString(
              s"Internal server error, expected error or data but got:\n${self.spaces2}"
            )
          )
        )
      )

    def toStreamingMessage(id: String): FromServer =
      dataField.fold(errorsField.fold(unexpectedErrorMessage(id))(Error(id, _))) { d =>
        Data(id, DataWrapper(d, errorsField))
      }

object json extends JsonSyntax
