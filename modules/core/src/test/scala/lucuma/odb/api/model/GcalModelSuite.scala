// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class GcalModelSuite extends DisciplineSuite {

  import ArbGcalModel._

  checkAll("GcalModel",        EqTests[GcalModel].eqv)
  checkAll("GcalModel.Create", EqTests[GcalModel.Create].eqv)

}