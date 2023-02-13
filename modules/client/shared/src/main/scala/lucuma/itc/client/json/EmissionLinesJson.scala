// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.SpectralDefinition.EmissionLines

given [T]: Encoder[EmissionLines[T]] with
  def apply(el: EmissionLines[T]): Json =
    Json.obj(
      "lines"                -> Json.arr(el.lines.toList.map { case (w, l) =>
        Json.obj(
          "wavelength" -> w.asJson,
          "lineWidth"  -> l.lineWidth.value.value.value.asJson,
          "lineFlux"   ->
            Json.obj(
              "value" -> l.lineFlux.value.value.value.asJson,
              "units" -> l.lineFlux.units.serialized.asJson
            )
        )
      }: _*),
      "fluxDensityContinuum" ->
        Json.obj(
          "value" -> el.fluxDensityContinuum.value.value.value.asJson,
          "units" -> el.fluxDensityContinuum.units.serialized.asJson
        )
    )
