// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.syntax.all._
import io.circe.literal._
import lucuma.core.enums._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.ItcObservingConditions

class GraphQLImagingTimeSuite extends GraphQLSuite {

  test("gmos north case") {
    query(
      """
        query {
          imagingIntegrationTime(input: {
            mode: {
              gmosN: {
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
                resolution
                params {
                  ... on GmosNITCParams {
                    grating
                  }
                }
                wavelength {
                  nanometers
                }
              }
            }
            result {
              exposures
              exposureTime {
                seconds
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
                "instrument" : "GMOS_NORTH",
                "resolution" : 970,
                "params": {
                  "grating": "B1200_G5301"
                },
                "wavelength" : {
                  "nanometers" : 60.000
                }
              },
              "result" : {
                "exposures" : 10,
                "exposureTime" : {
                  "seconds" : 1.000000000
                }
              }
            }
          }
        }
        """
    )
  }
}
// wavelength: {
//   nanometers: 60,
// },
// radialVelocity: {
//   kilometersPerSecond: 1000
// },
// signalToNoise: 2,
// sourceProfile: {
//   point: {
//     bandNormalized: {
//       sed: {
//         stellarLibrary: O5_V
//       }
//       brightnesses: [ {
//         band: R
//         value: 3
//         units: ERG_PER_S_PER_CM_SQUARED_PER_A
//       }, {
//         band: J
//         value: 2.1
//         units: AB_MAGNITUDE
//       }]
//     }
//   }
// },
// band: J,
// constraints: {
//   imageQuality: POINT_THREE,
//   cloudExtinction: POINT_FIVE,
//   skyBackground: DARK,
//   waterVapor: DRY,
//   elevationRange: {
//     airMass: {
//       min: 1,
//       max: 2
//     }
//   }
// },
