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
    ItcMapping[IO].map(m => ItcService.routes[IO](ItcService.service[IO](m, null)))
  val itcFixture                  = ResourceSuiteLocalFixture(
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
      .flatMap(_.as[Json])
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
              picometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              nonStellar: QS02
            },
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
        "errors": [
          {"message": "Wavelength defined with multiple units {picometers, nanometers}"}
        ]
      }"""
    )
  }

  test("bad redshift") {
    query(
      """
        query {
          spectroscopy(input: {
            wavelength: {
              picometers: 300
            },
            redshift: "0.1",
            simultaneousCoverage: {
              picometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              stellar: A0III
            },
            magnitude: {
              band: UC,
              system: AB,
              value: 5
            }
          })
          {
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
        "errors": [
          {"message": "Redshift value is not valid StringValue(0.1)"}
        ]
      }"""
    )
  }

  test("bad redshift and wavelength") {
    query(
      """
        query {
          spectroscopy(input: {
            wavelength: {
              picometers: 300,
              nanometers: 200
            },
            redshift: "0.1",
            simultaneousCoverage: {
              picometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              stellar: A0III
            },
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
        "errors": [
          {"message": "Wavelength defined with multiple units {picometers, nanometers}"},
          {"message": "Redshift value is not valid StringValue(0.1)"}
        ]
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
            spectralDistribution: {
              blackBody: {
                temperature: 50.1
              }
            },
            magnitude: {
              band: AP,
              value: 5,
              error: 1.2,
              system: JY
            }
          }) {
            results {
              mode {
                wavelength {
                  nanometers
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
      json"""
        {
          "data": {
            "spectroscopy": {
              "results": [
                {
                  "mode": {
                    "wavelength": {
                      "nanometers": 1.0
                    }
                  },
                  "itc": {
                      "exposures": 10
                  }
                }
              ]
            }
          }
        }
      """
    )
  }

  test("multiple spectral distribution") {
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
            spectralDistribution: {
              blackBody: {
                temperature: 50.1
              },
              powerLaw: {
                index: 100
              }
            },
            magnitude: {
              band: Y,
              system: AB,
              value: 5
            }
          }) {
            results {
              mode {
                wavelength {
                  nanometers
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
      json"""
      {
        "errors": [
          {"message": "Spectral distribution value is not valid {blackBody, powerLaw}"}
        ]
      }
      """
    )
  }

  test("gaussian source") {
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
              sourceType: GAUSSIAN_SOURCE,
              fwhm: {
                microarcseconds: 1000
              }
            },
            spectralDistribution: {
              powerLaw: {
                index: 1000
              }
            },
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
