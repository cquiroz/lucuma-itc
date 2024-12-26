// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class GraphQLImagingTimeSuite extends GraphImagingQLSuite {

  test("gmos north case") {
    query(
      """
        query {
          imagingIntegrationTime(input: {
            atWavelength: {
              picometers: 530000
            },
            signalToNoise: 600,
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      },
                      brightnesses: [
                        {
                          band: SLOAN_I,
                          value: 9.484,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: B,
                          value: 8.116,
                          units: VEGA_MAGNITUDE
                        },
                        {
                          band: V,
                          value: 12.323,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: J,
                          value: 14.442,
                          units: VEGA_MAGNITUDE,
                          error: 0.018
                        },
                        {
                          band: H,
                          value: 9.798,
                          units: VEGA_MAGNITUDE,
                          error: 0.029
                        },
                        {
                          band: K,
                          value: 10.65,
                          units: VEGA_MAGNITUDE,
                          error: 0.03
                        }
                      ]
                    }
                  }
                },
                radialVelocity: {
                  metersPerSecond: 7560
                }
              }
            ],
            constraints: {
              imageQuality: TWO_POINT_ZERO,
              cloudExtinction: THREE_POINT_ZERO,
              skyBackground: BRIGHT,
              waterVapor: WET,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNImaging: {
                filter: G_PRIME
              }
            }
          }) {
            mode {
              __typename
              ... on ImagingMode {
                instrument
                params {
                  ... on GmosNITCParams {
                    filter
                  }
                }
              }
            }
            brightest {
              all {
                exposureCount
                exposureTime {
                  seconds
                }
                totalSignalToNoise
                singleSignalToNoise
              }
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
                totalSignalToNoise
                singleSignalToNoise
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "imagingIntegrationTime" : {
              "mode" : {
                "__typename" : "ImagingMode",
                "instrument" : "GMOS_NORTH",
                "params": {
                  "filter": "G_PRIME"
                }
              },
              "brightest" : {
                "all" : [{
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  },
                  "totalSignalToNoise": 10.000,
                  "singleSignalToNoise": 20.000
                }, {
                  "exposureCount" : 5,
                  "exposureTime" : {
                    "seconds" : 2.000000
                  },
                  "totalSignalToNoise": 20.000,
                  "singleSignalToNoise": 21.000
                }],
                "selected" : {
                  "exposureCount" : 5,
                  "exposureTime" : {
                    "seconds" : 2.000000
                  },
                  "totalSignalToNoise": 20.000,
                  "singleSignalToNoise": 21.000
                }
              }
            }
          }
        }
        """
    )
  }
}
