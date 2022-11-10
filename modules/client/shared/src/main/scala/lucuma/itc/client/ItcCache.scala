// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Applicative
import cats.Monad
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.Semaphore
import cats.syntax.all.*
import java.time.Duration
import java.time.Instant
import scala.util.Try

trait ItcCache[F[_], K, V] {

  def get(key: K): F[Option[V]]

  def put(key: K)(value: V): F[Unit]

  def getOrCalc(key: K)(value: => V): F[V]

  def getOrCalcF(key: K)(value: => F[V]): F[V]

  def remove(key: K): F[Unit]

  def flush: F[Unit]

}

object ItcCache {

  def simple[F[_]: Sync, K, V]: Resource[F, ItcCache[F, K, V]] = {

    def cache(ref: Ref[F, Map[K, V]]): ItcCache[F, K, V] =
      new ItcCache[F, K, V] {

        override def get(key: K): F[Option[V]] =
          ref.get.map(_.get(key))

        override def put(key: K)(value: V): F[Unit] =
          ref.update(_.updated(key, value))

        override def getOrCalc(key: K)(value: => V): F[V] =
          ref.modify { m =>
            m.get(key).fold((m.updated(key, value), m(key)))((m, _))
          }

        override def getOrCalcF(key: K)(value: => F[V]): F[V] =
          for {
            ov <- get(key)
            v  <- ov.fold(value.flatTap(put(key)))(_.pure)
          } yield v

        override def remove(key: K): F[Unit] =
          ref.update(_.removed(key))

        override def flush: F[Unit] =
          ref.set(Map.empty[K, V])

      }

    Resource.eval(Ref.of[F, Map[K, V]](Map.empty[K, V]).map(cache))

  }


}
