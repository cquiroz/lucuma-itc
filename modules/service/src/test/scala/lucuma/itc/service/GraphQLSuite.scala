// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.effect._
import org.http4s._
import org.http4s.syntax.all._
import org.http4s.circe._
import io.circe.Json
import io.circe.literal._

class GraphQLSuite extends munit.CatsEffectSuite {
  val service: IO[HttpRoutes[IO]] =
    ItcMapping[IO].map(m => ItcService.routes[IO](ItcService.service[IO](m)))
  val itcFixture = ResourceSuiteLocalFixture(
    "itc",
    Resource.make(service)(_ => IO.unit)
  )

  override def munitFixtures = List(itcFixture)

  test("empty post") {
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(Request(method = Method.POST, uri = uri"/itc"))
      }
      .intercept[MalformedMessageBodyFailure]
  }

  def query(query: String, expected: Json): IO[Unit] =
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(
          Request(method = Method.POST, uri = uri"/itc")
            .withEntity(Json.obj("query" -> Json.fromString(query)))
        )
      }
      .flatMap(_.as[Json]) //compile.toVector)
      .assertEquals(expected)

  test("multiple wv units") {
    query(
      """
        query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 60,
              picometers: 300
            },
            redshift: 0.1,
            simultaneousCoverage: {
              nanometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: STELLAR,
            magnitude: {
              band: Y,
              system: AB,
              value: 5
            }
          }) {
            results {
              itc {
                ... on ItcSuccess {
                  exposureTime
                }
              }
            }
          }
        }
        """,
      json"""{
        "errors": [{"message": "Multiple defined wavelength values"}]
      }"""
    )
  }

  test("default case") {
    query(
      """
        query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 60,
            },
            redshift: 0.1,
            simultaneousCoverage: {
              nanometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: STELLAR,
            magnitude: {
              band: Y,
              system: AB,
              value: 5
            }
          }) {
            results {
              mode {
                wavelength {
                  picometers
                }
              }
              itc {
                ... on ItcSuccess {
                  exposures
                }
              }
            }
          }
        }
        """,
      json"""{
        "data": {
          "spectroscopy": {
            "results": [
              {
                "mode": {
                  "wavelength": {
                    "picometers": 1000
                  }
                },
                "itc": {
                  "exposures": 10
                }
              }
            ]
          }
        }
      }"""
    )
  }
}
