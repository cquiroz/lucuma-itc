// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal._

class GraphQLSpectroscopySuite extends GraphQLSuite {

  test("gmos north case") {
    query(
      """
        query {
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
        }
        """,
      json"""
        {
          "data": {
            "spectroscopy" : [
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5301"
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "itc" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
                  }
                ]
              },
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5301"
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "itc" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
                  }
                ]
              }
            ]
          }
        }
        """
    )
  }
}
