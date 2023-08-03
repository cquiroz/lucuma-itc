// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.GmosCcdModeInput

case class GmosNImagingInput(
  filter:  GmosNorthFilter,
  ccdMode: Option[GmosCcdMode]
) extends InstrumentModesInput

object GmosNImagingInput {

  def binding: Matcher[GmosNImagingInput] =
    ObjectFieldsBinding.rmap {
      case List(GmosNorthFilterBinding("filter", filter),
                GmosCcdModeInput.Binding.Option("ccdMode", ccdMode)
          ) =>
        (filter, ccdMode).parMapN(apply)
    }

}
