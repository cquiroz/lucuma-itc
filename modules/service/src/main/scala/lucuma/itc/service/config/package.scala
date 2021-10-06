// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import ciris.env
import ciris.prop
import ciris.ConfigValue

package object config {
  def envOrProp[F[_]](name: String): ConfigValue[F, String] =
    env(name).or(prop(name))

}
