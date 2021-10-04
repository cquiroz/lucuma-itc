// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import lucuma.core.math.Redshift
import lucuma.core.model.Magnitude
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution

/** Target properties we need to know at phase zero. */
final case class TargetProfile(
  spatialProfile:       SpatialProfile,
  spectralDistribution: SpectralDistribution,
  magnitude:            Magnitude,
  redshift:             Redshift
)
