// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class GraphQLSpectroscopyTimeEmissionLineSuite extends GraphQLEmissionLineSuite {

  test("emission lines") {
    query(
      """
        query {
          spectroscopyIntegrationTime(input: {
            atWavelength: {
              nanometers: 60,
            },
            signalToNoise: 2,
            asterism: [
              {
                sourceProfile: {
                  point: {
                    emissionLines: {
                      lines: [
                        {
                          wavelength: {
                            picometers: 650000
                          },
                          lineWidth: 1,
                          lineFlux: {
                            value: 0.5,
                            units: W_PER_M_SQUARED
                          }
                        }
                      ],
                      fluxDensityContinuum: {
                        value: 0.5,
                        units: W_PER_M_SQUARED_PER_UM
                      }
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
              emissionLine { picometers }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "spectroscopyIntegrationTime" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "params": {
                  "grating": "B1200_G5301"
                },
                "centralWavelength" : {
                  "nanometers" : 60.000
                }
              },
              "brightest": {
                "selected" : {
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                },
                "emissionLine": {
                  "picometers": 650000
                }
              }
            }
          }
        }
        """
    )
  }
}
