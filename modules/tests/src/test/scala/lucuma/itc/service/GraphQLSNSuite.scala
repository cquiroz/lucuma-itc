// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class GraphQLSNSuite extends GraphQLSuite {

  test("calculate SN") {
    query(
      """
        query {
          spectroscopySignalToNoise(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            exposureTime: {
              milliseconds: 2.5,
            },
            exposures: 10,
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
            band: J,
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
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            },
            significantFigures: {
              xAxis: 4
            }
          }) {
            resultType
            ... on SNCalcSuccess {
              signalToNoise
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopySignalToNoise": {
              "resultType": "SUCCESS",
              "signalToNoise": 1001.0
            }
          }
        }
        """
    )
  }

  test("calculate SN at (exact)") {
    query(
      """
        query {
          spectroscopySignalToNoise(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            exposureTime: {
              milliseconds: 2.5,
            },
            exposures: 10,
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
            band: J,
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
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            },
            significantFigures: {
              xAxis: 4
            }
            signalToNoiseAt: {
              nanometers: 1.0,
            },
          }) {
            resultType
            ... on SNCalcSuccess {
              signalToNoise
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopySignalToNoise": {
              "resultType": "SUCCESS",
              "signalToNoise": 1000.0
            }
          }
        }
        """
    )
  }

  test("calculate SN at (interpolated)") {
    query(
      """
        query {
          spectroscopySignalToNoise(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            exposureTime: {
              milliseconds: 2.5,
            },
            exposures: 10,
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
            band: J,
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
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            },
            significantFigures: {
              xAxis: 4
            }
            signalToNoiseAt: {
              nanometers: 1.5,
            },
          }) {
            resultType
            ... on SNCalcSuccess {
              signalToNoise
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopySignalToNoise": {
              "resultType": "SUCCESS",
              "signalToNoise": 1000.5
            }
          }
        }
        """
    )
  }
  test("at below range") {
    query(
      """
        query {
          spectroscopySignalToNoise(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            exposureTime: {
              milliseconds: 2.5,
            },
            exposures: 10,
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
            band: J,
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
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            },
            significantFigures: {
              xAxis: 4
            }
            signalToNoiseAt: {
              nanometers: 0.1
            },
          }) {
            resultType
            ... on SNWavelengthAtBelowRange {
              signalToNoiseAt {
                nanometers
              }
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopySignalToNoise": {
              "resultType": "BELOW_RANGE",
              "signalToNoiseAt": {
                "nanometers": 0.10
              }
            }
          }
        }
        """
    )
  }

  test("at above range") {
    query(
      """
        query {
          spectroscopySignalToNoise(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            exposureTime: {
              milliseconds: 2.5,
            },
            exposures: 10,
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
            band: J,
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
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            },
            significantFigures: {
              xAxis: 4
            }
            signalToNoiseAt: {
              nanometers: 5.1,
            },
          }) {
            resultType
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopySignalToNoise": {
              "resultType": "ABOVE_RANGE"
            }
          }
        }
        """
    )
  }
}
