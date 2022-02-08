// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import cats._
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enum._
import lucuma.itc.Itc
import lucuma.itc.ItcObservingConditions
import lucuma.itc.search.gmosnorth.GmosNorthFilterSelector

sealed trait Result {
  def mode: ObservingMode
  def itc: Itc.Result
}

object Result {
  case class Spectroscopy(mode: ObservingMode.Spectroscopy, itc: Itc.Result)
}

final case class SpectroscopyResults(results: List[Result.Spectroscopy])

object Search {

  def spectroscopy[F[_]: Parallel: Monad: Itc](
    constraints:    Constraints.Spectroscopy,
    targetProfile:  TargetProfile,
    itcConstraints: ItcObservingConditions,
    signalToNoise:  PosInt
  ): F[SpectroscopyResults] = {

    // As a first pass we'll generate every possible configuration and then filter them at the end.
    // This lets us apply the constraints in one place rather than duplicating the filtering logic
    // for each instrument (at the cost of dealing with some large sets in memory).

    val excludedFPUs: Set[GmosNorthFpu] = {
      import GmosNorthFpu._
      Set(Ifu2Slits, IfuBlue, IfuRed, Ns0, Ns1, Ns2, Ns3, Ns4, Ns5)
    }

    val gmosNorthModes: List[ObservingMode.Spectroscopy] =
      for {
        disp   <- GmosNorthDisperser.all
        fpu    <- GmosNorthFpu.all.filterNot(excludedFPUs)
        filter <- GmosNorthFilterSelector.selectBlocking(disp, fpu, constraints.λ).toList
      } yield ObservingMode.Spectroscopy.GmosNorth(constraints.λ, disp, fpu, filter)

    // more instruments ...

    // Every spectrographic observing mode
    val allModes: List[ObservingMode.Spectroscopy] =
      gmosNorthModes // ++ ...

    // Now filter down the list.
    val compatibleModes: List[ObservingMode.Spectroscopy] =
      allModes
        .filter(_.coverage.width >= constraints.simultaneousCoverage)
        .filter(_.resolution >= constraints.resolution.value)

    // Done!
    val resp = compatibleModes
      .parTraverse { mode =>
        Itc[F]
          .calculate(targetProfile, mode, itcConstraints, signalToNoise.value)
          .map(Result.Spectroscopy(mode, _))
      }
      .map(_.sortBy {
        case Result.Spectroscopy(_, Itc.Result.Success(t, n, _))    => t.toSeconds.toDouble * n
        case Result.Spectroscopy(_, Itc.Result.SourceTooBright(_))  => Double.MaxValue
        case Result.Spectroscopy(_, Itc.Result.CalculationError(_)) => Double.MaxValue
      })
      .map(SpectroscopyResults(_))
    resp

  }

}
