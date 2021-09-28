// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import cats._
import cats.syntax.all._

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import edu.gemini.grackle.circe.CirceMapping

import Query._, Path._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.{ mkErrorResult, mkOneError }
import generic._, semiauto._
import io.circe.Json
import io.circe.Encoder
import scala.util.Using
import scala.io.Source
import cats.effect.{ Unique => _, _ }
import natchez.Trace
import lucuma.odb.search.SpectroscopyResults
import lucuma.odb.search.Result.Spectroscopy
import lucuma.odb.search.ObservingMode
import lucuma.odb.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.odb.itc.Itc
import lucuma.core.math.Wavelength
import scala.concurrent.duration.FiniteDuration

trait Encoders {
  import io.circe.generic.semiauto._
  implicit val encoderFiniteDuration: Encoder[FiniteDuration] = new Encoder[FiniteDuration] {
    final def apply(d: FiniteDuration): Json = Json.obj(
        ("seconds", Json.fromLong(d.toSeconds))
      )
  }
  implicit val encoderItcResult: Encoder[Itc.Result] =
    deriveEncoder[Itc.Result]
  implicit val encoderWavelength: Encoder[Wavelength] = new Encoder[Wavelength] {
  final def apply(w: Wavelength): Json = Json.obj(
      ("nanometer", Json.fromInt(w.nanometer.value.toInt))
    )
  }
  implicit val encoderGmosNorth: Encoder[GmosNorth] =
    deriveEncoder[GmosNorth]
  implicit val encoderObservingMode: Encoder[ObservingMode.Spectroscopy] =
    deriveEncoder[ObservingMode.Spectroscopy]
  implicit val encoderSpectroscopy: Encoder[Spectroscopy] =
    deriveEncoder[Spectroscopy]

  implicit val encoderSpectroscopyResults: Encoder[SpectroscopyResults] =
    deriveEncoder[SpectroscopyResults]
}

object ItcMapping extends Encoders {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F].defer {
      Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
        Schema(src.mkString).right.get
      }.liftTo[F]
    }

  def computeItc[F[_]](env: Cursor.Env): F[Result[SpectroscopyResults]] = {
    println(env)
    println("abc1")
    println(env.get[Int]("wavelength"))
    println("abc")
    ???
  }

  def apply[F[_]: Sync]: F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] with ComputeMapping[F] {

      val schema: Schema = loadedSchema
      val QueryType = schema.ref("Query")
      val SpectroscopyResultType = schema.ref("spectroscopyResults")
      val typeMappings =
        List(
          ObjectMapping(
            tpe = QueryType,
            fieldMappings = List(
              ComputeRoot[SpectroscopyResults]("spectroscopy", ScalarType.StringType, computeItc)
            )
          ),
      )
      override val selectElaborator = new SelectElaborator(Map(
        QueryType -> {
          case s @ Select("spectroscopy", List(Binding("input", ObjectValue(wv))), child) =>//List(Binding("episode", TypedEnumValue(e))), child) =>
            println(wv)
            println(child)
            println("here")
            wv.foldLeft(Environment(Cursor.Env(), child).rightIor) {
              case (i, ("wavelength", ObjectValue(units))) if units.filter(_._2 == Value.AbsentValue).length != 1 =>
                i
              case (i, ("wavelength", ObjectValue(units))) if units.length != 1 =>
                println(s"$units")
                val r = i.map(e => e.copy(env = e.env.add(("wavelength", 1)), Select("spectroscopy", Nil, child)))
                println(r)
                r
              case (e, _) => e
            }//.rightIor
            // StandardRole.Id.parse(id).toRightIorNec(Problem(s"Not a valid role id: $id")).map { roleId =>
            //     Environment(
            //       Cursor.Env("roleId" -> roleId),
            //       Select("createApiKey", Nil, child)
            //     )
// List(Binding(input,ObjectValue(List((wavelength,ObjectValue(List((picometers,AbsentValue), (angstroms,AbsentValue), (nanometers,IntValue(600)), (micrometers,AbsentValue), (fromLong,AbsentValue), (fromDecimal,AbsentValue)))), (simultaneousCoverage,ObjectValue(List((picometers,AbsentValue), (angstroms,AbsentValue), (nanometers,IntValue(200)), (micrometers,AbsentValue), (fromLong,AbsentValue), (fromDecimal,AbsentValue)))), (resolution,IntValue(10)), (signalToNoise,IntValue(2)), (spatialProfile,ObjectValue(List((sourceType,TypedEnumValue(EnumValue(POINT_SOURCE,None,false,None))), (fwhm,AbsentValue)))), (spectralDistribution,TypedEnumValue(EnumValue(STELLAR,None,false,None))), (magnitude,ObjectValue(List((band,TypedEnumValue(EnumValue(Y,None,false,None))), (value,IntValue(5)), (error,AbsentValue), (system,TypedEnumValue(EnumValue(AB,None,false,None)))))), (redshift,FloatValue(0.1))))))
              // }

            // Select("spectroscopy", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const("abc")), child))).rightIor
        }))
            // LeafMapping[Beast](BeastType),
      }
  }
}
