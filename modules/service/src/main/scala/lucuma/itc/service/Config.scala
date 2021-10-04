// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import ciris.ConfigDecoder
import ciris.ConfigValue
import ciris.env
import ciris.prop
import org.http4s.Uri

/**
 * Application configuration.
 */
final case class Config(port: Int)

object Config {

  implicit val uri: ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

  def envOrProp[F[_]](name: String): ConfigValue[F, String] =
    env(name).or(prop(name))

  def fromCiris[F[_]]: ConfigValue[F, Config] =
    envOrProp("ITC_PORT")
      .or(envOrProp("PORT"))
      .or(ConfigValue.default("8080"))
      .as[Int]
      .map(Config.apply)

}
