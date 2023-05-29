// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.redis

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.*

import scala.concurrent.duration.*

// --------------------------------------------------
// Pickler to store results in binary with boopickle
// The data is further gzipped
// --------------------------------------------------

given picklerRefined[A: Pickler, B](using Validate[A, B]): Pickler[A Refined B] =
  new Pickler[A Refined B] {
    override def pickle(a: A Refined B)(using state: PickleState): Unit = {
      state.pickle(a.value)
      ()
    }
    override def unpickle(using state: UnpickleState): A Refined B      = {
      val value = state.unpickle[A]
      refineV[B](value).getOrElse(sys.error("Cannot unpickle"))
    }
  }

given picklerEnumerated[A: Enumerated]: Pickler[A] =
  transformPickler((a: String) => Enumerated[A].fromTag(a).getOrElse(sys.error("Cannot unpickle")))(
    Enumerated[A].tag(_)
  )

given picklerNonEmptyList[A: Pickler]: Pickler[NonEmptyList[A]] =
  transformPickler(NonEmptyList.fromListUnsafe[A])(_.toList)

given Pickler[ItcSeries]      =
  transformPickler(Function.tupled(ItcSeries.apply _))(x => (x.title, x.seriesType, x.data))
given Pickler[FiniteDuration] =
  transformPickler(n => new FiniteDuration(n, NANOSECONDS))(_.toNanos)

given Pickler[SignalToNoise] =
  transformPickler((bd: BigDecimal) =>
    SignalToNoise.FromBigDecimalExact
      .getOption(bd)
      .getOrElse(sys.error("cannot unpickle"))
  )(_.toBigDecimal)

given Pickler[TimeSpan] =
  transformPickler((d: Long) =>
    TimeSpan
      .fromMicroseconds(d)
      .getOrElse(sys.error("cannot unpickle"))
  )(_.toMicroseconds)

given Pickler[Wavelength] =
  transformPickler((i: Int) =>
    Wavelength.intPicometers
      .getOption(i)
      .getOrElse(sys.error("cannot unpickle"))
  )(_.toPicometers.value.value)

given Pickler[ItcChart]      = generatePickler
given Pickler[ItcChartGroup] = generatePickler
given Pickler[ItcWarning]    = generatePickler
given Pickler[ItcCcd]        = generatePickler
given Pickler[FinalSN]       = transformPickler((s: SignalToNoise) => FinalSN(s))(_.value)
given Pickler[SingleSN]      = transformPickler((s: SignalToNoise) => SingleSN(s))(_.value)
given Pickler[GraphResult]   = generatePickler

given Pickler[IntegrationTime] = generatePickler
