// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.{Applicative}
import cats.effect.Concurrent
// import cats.syntax.all._
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.extras.semiauto.deriveEnumerationDecoder
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.syntax.all._
import org.http4s.dsl._
import org.http4s.implicits._
import lucuma.itc.model._
import coulomb._
import coulomb.refined._
import eu.timepit.refined.numeric.Positive
import lucuma.core.math.Angle

// #service
trait ItcService[F[_]] {
  def runQuery(op: Option[String], vars: Option[Json], query: String): F[Unit]
}

trait ItcParametersCodec {
  import edu.gemini.spModel.core.{
    // SpectralDistribution,
    UniformSource,
    // SpatialProfile,
    // BrightnessUnit,
    // MagnitudeBand
  }
  import lucuma.core.math.Redshift
  import lucuma.core.math.Wavelength
  import lucuma.core.enum.MagnitudeBand
  import lucuma.core.model.SpatialProfile

  implicit val redshiftDefinitionDecoder: Decoder[Redshift] =
    deriveDecoder[Redshift]
  implicit val magnitudeBandDecoder: Decoder[MagnitudeBand] =
    deriveDecoder[MagnitudeBand]
  implicit val wavelengthDecoder: Decoder[Wavelength] =
    Decoder.decodeInt.emap(x => Either.fromOption(Wavelength.fromPicometers.getOption(x), "Failure"))
  // implicit val brigTnessUnitDecoder: Decoder[BrightnessUnit] = ???
  implicit val widthDecoder: Decoder[EmissionLine.Width] =
    Decoder.decodeBigDecimal.emap(x => Either.catchNonFatal(x.withRefinedUnit[Positive, EmissionLine.KPS]).leftMap(_.getMessage))
  implicit val irradianceDecoder: Decoder[EmissionLine.Irradiance] =
    Decoder.decodeBigDecimal.emap(x => Either.catchNonFatal(x.withRefinedUnit[Positive, EmissionLine.WSM]).leftMap(_.getMessage))
  implicit val spectralIrradianceDecoder: Decoder[EmissionLine.SpectralIrradiance] =
    Decoder.decodeBigDecimal.emap(x => Either.catchNonFatal(x.withRefinedUnit[Positive, EmissionLine.WSMM]).leftMap(_.getMessage))
  implicit val emissionDecoder: Decoder[EmissionLine] =
    deriveDecoder[EmissionLine]
  implicit val powerLawDecoder: Decoder[PowerLaw] =
    deriveDecoder[PowerLaw]
  implicit val userDefinedSpectrumDecoder: Decoder[UserDefinedSpectrum] =
    deriveDecoder[UserDefinedSpectrum]
  implicit val spectralDistrubutionDecoder: Decoder[SpectralDistribution] =
    deriveDecoder[SpectralDistribution]
  implicit val angleDecoder: Decoder[Angle] =
    Decoder.decodeLong.emap(x => Angle.microarcseconds.reverseGet(x).asRight)
  implicit val gaussianDecoder: Decoder[SpatialProfile.GaussianSource] =
    deriveDecoder[SpatialProfile.GaussianSource]
  implicit val uniformSourceDecoder: Decoder[SpatialProfile.UniformSource.type] =
    deriveDecoder[SpatialProfile.UniformSource.type]
  implicit val pointSourceDecoder: Decoder[SpatialProfile.PointSource.type] =
    deriveDecoder[SpatialProfile.PointSource.type]
  implicit val spatialProfileDecoder: Decoder[SpatialProfile] =
    List[Decoder[SpatialProfile]](
      Decoder[SpatialProfile.PointSource.type].widen,
      Decoder[SpatialProfile.UniformSource.type].widen,
    ).reduceLeft(_ or _)
  implicit val sourceDefinitionDecoder: Decoder[SourceDefinition] =
    deriveDecoder[SourceDefinition]
  implicit val paramsDecoder: Decoder[ItcParameters] =
    deriveDecoder[ItcParameters]
}

object ItcService extends ItcParametersCodec {
  def routes[F[_]: Concurrent](service: ItcService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    //
    //   implicit val jsonQPDecoder: QueryParamDecoder[Json] =
    //     QueryParamDecoder[String].emap { s =>
    //       parser.parse(s).leftMap { case ParsingFailure(msg, _) =>
    //         ParseFailure("Invalid variables", msg)
    //       }
    //     }
    //
    //   object QueryMatcher extends QueryParamDecoderMatcher[String]("query")
    //   object OperationNameMatcher
    //       extends OptionalQueryParamDecoderMatcher[String]("operationName")
    //   object VariablesMatcher
    //       extends OptionalValidatingQueryParamDecoderMatcher[Json]("variables")
    //
    HttpRoutes.of[F] {
      //     // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / "json" =>
        for {
          body <- req.as[ItcParameters]
          resp <- Ok("ok")
        } yield resp
      //       for {
      //         body <- req.as[Json]
      //         obj <- body.asObject.liftTo[F](
      //           InvalidMessageBodyFailure("Invalid GraphQL query")
      //         )
      //         query <- obj("query")
      //           .flatMap(_.asString)
      //           .liftTo[F](InvalidMessageBodyFailure("Missing query field"))
      //         op = obj("operationName").flatMap(_.asString)
      //         vars = obj("variables")
      //         result <- service.runQuery(op, vars, query)
      //         resp <- Ok(result)
      //       } yield resp
    }
  }
  //
  def service[F[_]](implicit F: Applicative[F]): ItcService[F] =
    new ItcService[F] {
      def runQuery(
          op: Option[String],
          vars: Option[Json],
          query: String
      ): F[Unit] =
        Applicative[F].unit
      // StarWarsMapping.compileAndRun(query, op, vars).pure[F]
    }
}
// #service
