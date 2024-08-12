// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.itc.encoders.given

case class UpstreamException(msg: List[String]) extends RuntimeException(msg.mkString("\n"))

enum SNResultType(val tag: String) derives Enumerated:
  case Success          extends SNResultType("success")
  case SourceTooBright  extends SNResultType("source_too_bright")
  case BelowRange       extends SNResultType("below_range")
  case AboveRange       extends SNResultType("above_range")
  case NoData           extends SNResultType("no_data")
  case CalculationError extends SNResultType("calculation_error")

sealed trait SNCalcResult extends Product with Serializable {
  def resultType: SNResultType
}

object SNCalcResult:
  given Encoder[SNCalcResult] = Encoder.instance { a =>
    Json
      .obj(("resultType", a.resultType.asJson))
      .deepMerge(a match {
        case s @ SNCalcSuccess(_)          => s.asJson
        case NoData()                      => Json.Null
        case w @ WavelengthAtAboveRange(_) => w.asJson
        case w @ WavelengthAtBelowRange(_) => w.asJson
        case _                             => Json.Null
      })
  }

  case class SNCalcSuccess(
    signalToNoise: SignalToNoise
  ) extends SNCalcResult
      derives Encoder.AsObject {
    val resultType = SNResultType.Success
  }

  case class NoData() extends SNCalcResult {
    val resultType = SNResultType.NoData
  }

  case class WavelengthAtBelowRange(signalToNoiseAt: Wavelength) extends SNCalcResult
      derives Encoder.AsObject {
    val resultType = SNResultType.BelowRange
  }

  case class WavelengthAtAboveRange(signalToNoiseAt: Wavelength) extends SNCalcResult
      derives Encoder.AsObject {
    val resultType = SNResultType.AboveRange
  }

  /** Generic calculation error */
  case class CalculationError(msg: String) extends SNCalcResult derives Encoder.AsObject {
    val resultType = SNResultType.CalculationError
  }
