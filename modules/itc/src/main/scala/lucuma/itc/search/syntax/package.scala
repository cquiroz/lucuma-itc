// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

package object syntax {
  object all
      extends ToGmosNorthFilterOps
      with ToGmosNorthDisperserOps
      with ToGmosNorthFpuOps
      with ToConditionsOps

}
