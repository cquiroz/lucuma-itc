// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.Order
import cats.derived.*
import io.circe.Encoder
import io.circe.generic.semiauto.*

case class TargetTimeAndGraphs(
  integrationTime: TargetIntegrationTime,
  graphs:          TargetGraphs
) derives Eq,
      Encoder.AsObject

object TargetTimeAndGraphs:
  val integrationTimeOrder: Order[TargetTimeAndGraphs] =
    given Order[TargetIntegrationTime] = TargetIntegrationTime.exposureTimeOrder
    Order.by(_.integrationTime)
