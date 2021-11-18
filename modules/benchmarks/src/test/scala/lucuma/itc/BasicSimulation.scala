package lucuma.itc

// import scala.concurrent.duration._
//
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import lucuma.itc.service.Main
import cats.effect.unsafe.implicits.global
import scala.concurrent.Future
import io.circe.Json

class BasicSimulation extends Simulation {
  var cancelToken: Option[() => Future[Unit]] = None
  before {
    System.setProperty("PORT", "5000")
    System.setProperty("ITC_URL", "https://gemini-new-itc.herokuapp.com/json")
    cancelToken = Some(Main.run(Nil).unsafeRunCancelable())
  }

  after {
    cancelToken.foreach(c => c())
  }
  val headers_10   = Map("Content-Type" -> """application/json""")
  val httpProtocol = http.baseUrl("http://127.0.0.1:5000")

  val queryStr = """query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 60,
            },
            redshift: 0.1,
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
            },
            constraints: {
              imageQuality: POINT_THREE,
              cloudExtinction: POINT_FIVE,
              skyBackground: DARK,
              waterVapor: DRY,
              elevationRange: {
                airmassRange: {
                  min: 1,
                  max: 2
                }
              }
            },
            modes: [{
              gmosN: {
                filter: G_PRIME,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }, {
              gmosN: {
                filter: GG455,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }
            ]
          }) {
            results {
                mode {
                  instrument
                  resolution
                  params {
                    ... on GmosNITCParams {
                      disperser
                    }
                  }
                  wavelength {
                    nanometers
                  }
                }
                itc {
                  ... on ItcSuccess {
                    exposures
                    exposureTime {
                      seconds
                    }
                  }
                }
            }
          }
        }"""

  val body = Json.obj("query" -> Json.fromString(queryStr))

  val scn = scenario("BasicSimulation")
    .pause(1)
    .exec(
      http("request_1")
        .post("/itc")
        .headers(headers_10)
        .body(StringBody(body.noSpaces))
    )

  setUp(
    scn.inject(atOnceUsers(1))
  ).protocols(httpProtocol)
}
