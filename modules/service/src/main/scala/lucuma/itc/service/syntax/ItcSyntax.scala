// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import cats.data.*
import grackle.Problem
import grackle.Query.Environment
import lucuma.core.math.SignalToNoise
import lucuma.itc.FinalSN
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcChartGroup
import lucuma.itc.ItcSeries
import lucuma.itc.SignificantFigures
import lucuma.itc.SingleSN
import lucuma.itc.math.roundToSignificantFigures
import monocle.Focus
import monocle.std.these.*

import scala.annotation.targetName

trait ItcSyntax:

  extension [A](self: Option[A])
    def toRightIorNec[E](e: => E): Ior[NonEmptyChain[E], A] =
      self match
        case Some(a) => Ior.Right(a)
        case None    => Ior.Left(NonEmptyChain.of(e))

  extension [A](self: A)
    def leftIorNec[B]: Ior[NonEmptyChain[A], B] =
      Ior.Left(NonEmptyChain.of(self))

    def rightIorNec[E]: Ior[NonEmptyChain[E], A] =
      Ior.Right(self)

  extension [A](self: IorNec[Problem, A])
    def addProblem(problem: String): Ior[NonEmptyChain[Problem], A] =
      self.addLeft(NonEmptyChain.of(Problem(problem)))

  extension [A](self: IorNec[String, A])
    def leftProblems: Ior[NonEmptyChain[Problem], A] =
      self.leftMap(_.map(Problem(_)))

  extension (self: String)
    def fromScreamingSnakeCase: String =
      self.split("_").map(_.toLowerCase.capitalize).mkString("")

  def cursorEnv[A] = theseRight[A, Environment].andThen(Focus[Environment](_.env))

  def cursorEnvAdd[A, B](key: String, value: B): Ior[A, Environment] => Ior[A, Environment] =
    cursorEnv[A].modify(_.add((key, value)))

end ItcSyntax

trait ItcChartSyntax:
  extension (series: ItcSeries)
    def adjustSignificantFigures(figures: SignificantFigures): ItcSeries =
      val data: List[(Double, Double)] =
        series.data.map((x, y) =>
          (figures.xAxis.fold(x)(xDigits => roundToSignificantFigures(x, xDigits.value)),
           figures.yAxis.fold(y)(yDigits => roundToSignificantFigures(y, yDigits.value))
          )
        )
      ItcSeries(series.title, series.seriesType, data)

  extension (chart: ItcChart)
    def adjustSignificantFigures(figures: SignificantFigures): ItcChart =
      chart.copy(series = chart.series.map(_.adjustSignificantFigures(figures)))

  extension (sn: FinalSN)
    @targetName("finalAdjust")
    def adjustSignificantFigures(figures: SignificantFigures): FinalSN =
      FinalSN(sn.value.adjustSignificantFigures(figures))

  extension (sn: SingleSN)
    @targetName("finalSingle")
    def adjustSignificantFigures(figures: SignificantFigures): SingleSN =
      SingleSN(sn.value.adjustSignificantFigures(figures))

  extension (sn: SignalToNoise)
    def adjustSignificantFigures(figures: SignificantFigures): SignalToNoise =
      figures.ccd match
        case Some(v) =>
          SignalToNoise.FromBigDecimalRounding
            .getOption(
              roundToSignificantFigures(sn.toBigDecimal, v.value)
            )
            .getOrElse(sn)
        case _       => sn

  extension (group: ItcChartGroup)
    def adjustSignificantFigures(figures: SignificantFigures): ItcChartGroup =
      group.copy(charts = group.charts.map(_.adjustSignificantFigures(figures)))

  extension (ccd: ItcCcd)
    def adjustSignificantFigures(figures: SignificantFigures): ItcCcd =
      figures.ccd.fold(ccd)(c =>
        ccd.copy(
          singleSNRatio = roundToSignificantFigures(ccd.singleSNRatio, c.value),
          maxSingleSNRatio = roundToSignificantFigures(ccd.maxSingleSNRatio, c.value),
          totalSNRatio = roundToSignificantFigures(ccd.totalSNRatio, c.value),
          maxTotalSNRatio = roundToSignificantFigures(ccd.maxTotalSNRatio, c.value),
          peakPixelFlux = roundToSignificantFigures(ccd.peakPixelFlux, c.value),
          wellDepth = roundToSignificantFigures(ccd.wellDepth, c.value),
          ampGain = roundToSignificantFigures(ccd.ampGain, c.value)
        )
      )

object all extends ItcSyntax with ItcChartSyntax
