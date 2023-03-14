// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import io.circe.parser.decode
import lucuma.itc.legacy
import lucuma.itc.legacy.given

/**
 * This class contains methods to call to methods in ItcCalculation via reflection. This is done
 * because the itc-server runs on scala 3 while the ItcCalculation method is based on scala 2
 *
 * Doing the call via reflection with a custom class loader lets us have both scala versions in the
 * jvm at the same time.
 *
 * Note that params are passed as a String for the same reason avoiding conflicts across classes
 * that may not be compatible. Instead we pass back and forth json encoded version of the params
 * essentially the same as if ITC were a server accepting json and responding json
 */
case class LocalItc(classLoader: ClassLoader):
  // We need to keep a single reference to the reflected method
  val calculateChartsMethod = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    .getMethod("calculateCharts", classOf[String], classOf[String])

  val calculateMethod = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    .getMethod("calculate", classOf[String], classOf[String])

  private val LegacyRight = """Right\((.*)\)""".r
  private val LegacyLeft  = """Left\((.*)\)""".r

  /**
   * This method does a call to the method ItcCalculation.calculateCharts via reflection.
   */
  def calculateCharts(jsonParams: String): Either[String, GraphsRemoteResult] =
    val res = calculateChartsMethod
      .invoke(null, jsonParams, "token") // null as it is a static method
      .asInstanceOf[String]

    res match
      case LegacyRight(result) =>
        decode[legacy.GraphsRemoteResult](result).leftMap { e =>
          e.getMessage()
        }
      case LegacyLeft(result)  =>
        Left(result)
      case m                   =>
        Left(s"Unknown result: $m")

  /**
   * This method does a call to the method ItcCalculation.calculate via reflection.
   */
  def calculateExposureTime(jsonParams: String): Either[String, ExposureTimeRemoteResult] =
    val res = calculateMethod
      .invoke(null, jsonParams, "token") // null as it is a static method
      .asInstanceOf[String]

    res match
      case LegacyRight(result) =>
        decode[legacy.ExposureTimeRemoteResult](result).leftMap { e =>
          e.getMessage()
        }
      case LegacyLeft(result)  =>
        Left(result)
      case m                   =>
        Left(s"Unknown result: $m")
