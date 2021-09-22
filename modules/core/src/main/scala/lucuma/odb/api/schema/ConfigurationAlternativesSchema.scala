// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import sangria.schema._
import sangria.macros.derive._
import sangria.marshalling.circe._
import lucuma.odb.api.model.ConfigurationAlternativesModel
import lucuma.odb.search.SpectroscopyResults
import lucuma.odb.search.Result
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.search.ObservingMode
import lucuma.odb.itc.Itc
import java.math.RoundingMode.HALF_UP

object ConfigurationAlternativesSchema {
  import WavelengthSchema._
  import RefinedSchema._
  import TargetMutation.InputObjectMagnitudeCreate
  import SpatialProfileSchema._
  import SpectralDistributionSchema._
  import syntax.inputobjecttype._

  val InputConfigurationAlternativesModelSearch: InputObjectType[ConfigurationAlternativesModel.SearchParameters] =
    deriveInputObjectType[ConfigurationAlternativesModel.SearchParameters](
      InputObjectTypeName("QueryConfigurationAlternativeSearchInput"),
      InputObjectTypeDescription("Configuration alternatives query"),
      DocumentInputField("wavelength", description  = "Observing wavelength."),
      DocumentInputField("simultaneousCoverage", description  = "Minimum desired simultaneous wavelength coverage."),
      DocumentInputField("resolution", description  = "Minimum desired resolution."),
      DocumentInputField("signalToNoise", description  = "Minimum desired signal-to-noise ratio."),
      DocumentInputField("spatialProfile", description  = "Spatial profile PointSource/UniformSource/GaussianSource."),
      DocumentInputField("spectralDistribution", description  = "Spectral distribution variant BlacBode/PowerLaw/Stellar/NonStellar."),
      DocumentInputField("magnitude", description  = "Target magnitude/system/band."),
      DocumentInputField("redshift", description  = "Target redshift.")
    )

  val ArgumentConfigurationAlternativesModelSearch: Argument[ConfigurationAlternativesModel.SearchParameters] =
    InputConfigurationAlternativesModelSearch.argument(
      "input",
      "Configuraton alternatives search parameters."
    )

  def SuccessItcResultType[F[_]]: ObjectType[OdbRepo[F], Itc.Result.Success] =
    ObjectType(
      name     = "ItcSuccess",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], Itc.Result.Success](ItcResultType[F])),
      fieldsFn = () => fields(

        Field(
          name        = "exposureTime",
          fieldType   = LongType,
          description = Some("Exposure time in milliseconds"),
          resolve     = _.value.exposureTime.toMillis
        ),

        Field(
          name        = "exposures",
          fieldType   = IntType,
          description = Some("Exposures"),
          resolve     = _.value.exposures
        ),

        Field(
          name        = "signalToNoise",
          fieldType   = IntType,
          description = Some("Signal/Noise ratio"),
          resolve     = _.value.signalToNoise
        ),

      )
    )

  def SourceTooBrightType[F[_]]: ObjectType[OdbRepo[F], Itc.Result.SourceTooBright] =
    ObjectType(
      name     = "ItcError",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], Itc.Result.SourceTooBright](ItcResultType[F])),
      fieldsFn = () => fields(

        Field(
          name        = "msg",
          fieldType   = StringType,
          description = Some("Message"),
          resolve     = _.value.msg
        ),

      )
    )

  def ItcResultType[F[_]]: InterfaceType[OdbRepo[F], Itc.Result] =
    InterfaceType[OdbRepo[F], Itc.Result](
      name         = "ItcResult",
      description  = "Possible itc result",
      fields[OdbRepo[F], Itc.Result](
        Field(
          name        = "resultType",
          fieldType   = StringType,
          description = Some("Result type"),
          resolve     = _.value match {
            case _: Itc.Result.Success => "ok"
            case _ => "fail"
          }
        )
      )
    ).withPossibleTypes(() => List(
      PossibleObject[OdbRepo[F], Itc.Result](SourceTooBrightType[F]),
      PossibleObject[OdbRepo[F], Itc.Result](SuccessItcResultType[F])
    ))
    // UnionType(
    //   name        = "Itc Result",
    //   description = Some("Either integration time values or an error"),
    //   types       = List(SuccessItcResultType[F], SourceTooBrightType[F])
    // ).mapValue[Either[Itc.Result.Success, Itc.Result.SourceTooBright.type]](
    //   _.fold(
    //     key => key: Any,
    //     st  => st: Any
    //   )
    // )
    //

  def ObservingModeSpectroscopyType[F[_]]: ObjectType[OdbRepo[F], ObservingMode.Spectroscopy] =
    ObjectType(
      name     = "ObservingModeSpectroscopy",
      fieldsFn = () => fields(

        Field(
          name        = "wavelength",
          fieldType   = WavelengthType[F],
          description = Some("Wavelength in appropriate units"),
          resolve     = _.value.λ
        ),

        Field(
          name        = "resolution",
          fieldType   = BigDecimalType,
          description = Some("Resolution"),
          resolve     = _.value.resolution.toBigDecimal(2, HALF_UP)
        ),

      )
    )

  def ResultSpectroscopyType[F[_]]: ObjectType[OdbRepo[F], Result.Spectroscopy] =
    ObjectType(
      name     = "Result",
      fieldsFn = () => fields(

        Field(
          name        = "mode",
          fieldType   = ObservingModeSpectroscopyType[F],
          description = Some("Spectroscopy mode"),
          resolve     = _.value.mode
        ),

        Field(
          name        = "itc",
          fieldType   = ItcResultType[F],
          description = Some("ITC results"),
          resolve     = _.value.itc
        ),

      )
    )

  def SpectroscopyResultsType[F[_]]: ObjectType[OdbRepo[F], SpectroscopyResults]=
    ObjectType(
      name     = "spectroscopyResult",
      fieldsFn = () => fields(

        Field(
          name        = "results",
          fieldType   = ListType(ResultSpectroscopyType[F]),
          description = Some("Search results"),
          resolve     = _.value.results
        ),

      )
    )
}
