// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.syntax.all._
import ciris._
import org.http4s.Uri

/**
 * Application configuration.
 */
final case class Config(
  environment: Environment,
  port:        Int,
  itcUrl:      Uri,
  honeycomb:   Option[HoneycombConfig]
)

object Config {

  implicit val uri: ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

  def config: ConfigValue[Effect, Config] =
    (envOrProp("LUCUMA_SSO_ENVIRONMENT")
       .as[Environment]
       .default(Environment.Local),
     envOrProp("ITC_PORT")
       .or(envOrProp("PORT"))
       .or(ConfigValue.default("8080"))
       .as[Int],
     envOrProp("ITC_URL").as[Uri],
     HoneycombConfig.config.option
    ).parMapN(Config.apply)

}
