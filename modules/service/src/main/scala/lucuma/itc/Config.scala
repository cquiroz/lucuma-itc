// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.effect.{Async, Resource}
import ciris.{ConfigValue, env, prop}
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.Client

/** Application configuration.
  */
final case class Config(
    port: Int
) {

  def httpClientResource[F[_]: Async]: Resource[F, Client[F]] =
    BlazeClientBuilder(
      scala.concurrent.ExecutionContext.Implicits.global
    ).resource

}

object Config {

  def envOrProp[F[_]](name: String): ConfigValue[F, String] =
    env(name) or prop(name)

  def fromCiris[F[_]]: ConfigValue[F, Config] = (
    (envOrProp("ITC_PORT") or envOrProp("PORT") or ConfigValue.default("8080"))
      .as[Int]
    )
    .map(Config.apply)

}
