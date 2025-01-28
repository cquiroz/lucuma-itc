// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class GraphQLIntTimeErrorsSuite extends FailingCalculationSuite {

  test("Test error channel") {
    query(
      """
        query {
          spectroscopyIntegrationTime(input: {
            atWavelength: {
              nanometers: 60
            },
            signalToNoise: 2,
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      }
                      brightnesses: [ {
                        band: R
                        value: 3
                        units: ERG_PER_S_PER_CM_SQUARED_PER_A
                      }, {
                        band: J
                        value: 2.1
                        units: AB_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 1000
                }
              }
            ],
            constraints: {
              imageQuality: POINT_THREE,
              cloudExtinction: POINT_FIVE,
              skyBackground: DARK,
              waterVapor: DRY,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                centralWavelength: {
                  nanometers: 60
                },
                filter: GG455,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
                params {
                  ... on GmosNITCParams {
                    grating
                  }
                }
                centralWavelength {
                  nanometers
                }
              }
            }
            brightest {
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
            }
          }
        }
        """,
      json"""
        {
          "errors": [{
            "message": "Error calculating ITC: A calculation error",
            "extensions": {
              "targetIndex": 0,
              "error": {
                "wellHalfFilledSeconds": null,
                "errorCode": "GENERAL",
                "message": "Error calculating ITC: A calculation error"
              }
            }
          }],
          "data": {
            "spectroscopyIntegrationTime" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "params" : {
                  "grating" : "B1200_G5301"
                },
                "centralWavelength" : {
                  "nanometers" : 60.000
                }
              },
              "brightest" : null
            }
          }
        }
        """
    )
  }
}
