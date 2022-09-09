// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.syntax.all._
import io.circe.literal._
import lucuma.core.enums._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.ItcObservingConditions

class GraphQLSpectroscopyGraphSuite extends GraphQLSuite {

  test("gmos graph") {
    query(
      """
        query {
          spectroscopyGraphBeta(input: {
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
              gmosN: {
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
            ccds {
              singleSNRatio
              totalSNRatio
              peakPixelFlux
              wellDepth
              ampGain
            }
            charts {
              chartType
              series {
                title
                seriesType
                data
                xAxis {
                  start
                  end
                  count
                }
                dataY
              }
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopyGraphBeta": {
              "ccds" : [
                {
                  "singleSNRatio" : 1.0,
                  "totalSNRatio" : 2.0,
                  "peakPixelFlux" : 3.0,
                  "wellDepth" : 4.0,
                  "ampGain" : 5.0
                }
              ],
              "charts": [
                {
                  "chartType": "S2N_CHART",
                  "series": [
                    {
                      "title": "title",
                      "seriesType": "BACKGROUND_DATA",
                      "data": [
                        [
                          1.0,
                          1000.0
                        ],
                        [
                          2.0,
                          1001.0
                        ]
                      ],
                      "xAxis" : {
                        "start" : 1.0,
                        "end" : 2.0,
                        "count" : 2
                      },
                      "dataY": [
                        1000.0,
                        1001.0
                      ]
                    }
                  ]
                }
              ]
            }
          }
        }
        """
    )
  }
}
