// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.syntax.all.*
import ciris.*
import org.http4s.Uri

/**
 * Application configuration.
 */
final case class Config(
  environment: ExecutionEnvironment,
  port:        Int,
  redisUrl:    Uri,
  honeycomb:   Option[HoneycombConfig],
  dyno:        Option[String]
)

object Config:

  given ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

  def config: ConfigValue[Effect, Config] =
    (envOrProp("LUCUMA_SSO_ENVIRONMENT")
       .as[ExecutionEnvironment]
       .default(ExecutionEnvironment.Local),
     envOrProp("ITC_PORT")
       .or(envOrProp("PORT"))
       .or(ConfigValue.default("6060"))
       .as[Int],
     envOrProp("REDISCLOUD_URL")
       .or(envOrProp("REDIS_URL"))
       .as[Uri],
     HoneycombConfig.config.option,
     env("DYNO").option
    ).parMapN(Config.apply)
