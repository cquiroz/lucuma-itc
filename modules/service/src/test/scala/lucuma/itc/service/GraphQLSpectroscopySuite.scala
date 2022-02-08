// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            signalToNoise: 2,
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

  test("gmos south case") {
    query(
      """
        query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              point: {
                bandNormalized: {
                  sed: {
                    planet: JUPITER
                  }
                  brightnesses: [ {
                    band: R
                    value: 3
                    units: ERG_PER_S_PER_CM_SQUARED_PER_A
                    error: 0.2
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
                airmassRange: {
                  min: 1,
                  max: 2
                }
              }
            },
            modes: [{
              gmosS: {
                filter: RG610,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5321
              }
            }, {
              gmosS: {
                filter: SII,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5321
              }
            }
            ]
          }) {
            results {
                mode {
                  instrument
                  resolution
                  params {
                    ... on GmosSITCParams {
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
                      "instrument" : "GMOS_SOUTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5321"
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
                      "instrument" : "GMOS_SOUTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5321"
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
//
//   test("gmos north case with variables") {
//     query(
//       """
//         query($input: SpectroscopyModeInput) {\n          spectroscopy(input: $input) {\n            results {\n              mode {\n                instrument\n              }\n            }\n          }\n        }\n
//       """,
//       """
//         {
//           "input" : {
//             "wavelength" : {
//               "nanometers" : "600"
//             },
//             "signalToNoise" : "2",
//             "spatialProfile" : {
//               "sourceType" : "POINT_SOURCE",
//               "fwhm" : null
//             },
//             "spectralDistribution" : {
//               "stellar": "A0I"
//             },
//             "magnitude" : {
//               "band" : "I",
//               "value" : "6",
//               "error" : null,
//               "system" : "VEGA"
//             },
//             "radialVelocity": {
//               "metersPerSecond": 1000
//             },
//             "constraints" : {
//               "imageQuality" : "POINT_EIGHT",
//               "cloudExtinction" : "POINT_FIVE",
//               "skyBackground" : "DARK",
//               "waterVapor" : "DRY",
//               "elevationRange" : {
//                 "airmassRange": {
//                   "min": "0.1",
//                   "max": "1.3"
//                 }
//               }
//             },
//             "modes": [{
//               "gmosN": {
//                 "filter": "G_PRIME",
//                 "fpu": "LONG_SLIT_0_25",
//                 "disperser": "B1200_G5301"
//               }
//             }, {
//               "gmosN": {
//                 "filter": "GG455",
//                 "fpu": "LONG_SLIT_0_25",
//                 "disperser": "B1200_G5301"
//               }
//             }
//             ]
//           }
//         }
//         """,
//       json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_NORTH"
//                     }
//                   }
//                 ]
//               },
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_NORTH"
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//     )
//   }
//
//   val allConditions =
//     for {
//       iq <- Enumerated[ImageQuality].all
//       ce <- Enumerated[CloudExtinction].all
//       wv <- Enumerated[WaterVapor].all
//       sb <- Enumerated[SkyBackground].all
//     } yield ItcObservingConditions(iq, ce, wv, sb, 2)
//
//   val conditions = ItcObservingConditions(ImageQuality.PointEight,
//                                           CloudExtinction.OnePointFive,
//                                           WaterVapor.Median,
//                                           SkyBackground.Bright,
//                                           2
//   )
//
//   test("iterate over conditions") {
//     allConditions.traverse { c =>
//       query(
//         s"""
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               centimetersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: ${c.iq.tag.toScreamingSnakeCase},
//               cloudExtinction: ${c.cc.tag.toScreamingSnakeCase},
//               skyBackground: ${c.sb.tag.toScreamingSnakeCase},
//               waterVapor: ${c.wv.tag.toScreamingSnakeCase},
//               elevationRange: {
//                 airmassRange: {
//                   min: 1,
//                   max: 2
//                 }
//               }
//             },
//             modes: [{
//               gmosN: {
//                 filter: G_PRIME,
//                 fpu: LONG_SLIT_0_25,
//                 disperser: B1200_G5301
//               }
//             }, {
//               gmosN: {
//                 filter: GG455,
//                 fpu: LONG_SLIT_0_25,
//                 disperser: B1200_G5301
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   resolution
//                   params {
//                     ... on GmosNITCParams {
//                       disperser
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//         json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_NORTH",
//                       "resolution" : 970,
//                       "params": {
//                         "disperser": "B1200_G5301"
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               },
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_NORTH",
//                       "resolution" : 970,
//                       "params": {
//                         "disperser": "B1200_G5301"
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//       )
//     }
//   }
//
//   test("Bad airmass") {
//     query(
//       """
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               metersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: POINT_THREE,
//               cloudExtinction: POINT_FIVE,
//               skyBackground: DARK,
//               waterVapor: DRY,
//               elevationRange: {
//                 airmassRange: {
//                   min: 2,
//                   max: 1
//                 }
//               }
//             },
//             modes: [{
//               gmosN: {
//                 filter: G_PRIME,
//                 fpu: LONG_SLIT_0_25,
//                 disperser: B1200_G5301
//               }
//             }, {
//               gmosN: {
//                 filter: GG455,
//                 fpu: LONG_SLIT_0_25,
//                 disperser: B1200_G5301
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   resolution
//                   params {
//                     ... on GmosNITCParams {
//                       disperser
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//       json"""
//         {
//           "errors": [
//             {
//               "message" : "Airmass max value 1 must be more than the min value 2"
//             }
//           ]
//         }
//         """
//     )
//   }
//
//   test("gmosN_dispersers") {
//     Enumerated[GmosNorthDisperser].all.traverse { d =>
//       query(
//         s"""
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               centimetersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: POINT_THREE,
//               cloudExtinction: POINT_FIVE,
//               skyBackground: DARK,
//               waterVapor: DRY,
//               elevationRange: {
//                 airmassRange: {
//                   min: 1,
//                   max: 2
//                 }
//               }
//             },
//             modes: [{
//               gmosN: {
//                 filter: G_PRIME,
//                 fpu: LONG_SLIT_0_25,
//                 disperser: ${d.tag.toScreamingSnakeCase}
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   params {
//                     ... on GmosNITCParams {
//                       disperser
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//         json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_NORTH",
//                       "params": {
//                         "disperser": ${d.tag.toScreamingSnakeCase}
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//       )
//     }
//   }
//
//   test("gmosS_dispersers") {
//     Enumerated[GmosSouthDisperser].all.traverse { d =>
//       query(
//         s"""
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               centimetersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: POINT_THREE,
//               cloudExtinction: POINT_FIVE,
//               skyBackground: DARK,
//               waterVapor: DRY,
//               elevationRange: {
//                 airmassRange: {
//                   min: 1,
//                   max: 2
//                 }
//               }
//             },
//             modes: [{
//               gmosS: {
//                 filter: G_PRIME,
//                 fpu: LONG_SLIT_0_25,
//                 disperser: ${d.tag.toScreamingSnakeCase}
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   params {
//                     ... on GmosSITCParams {
//                       disperser
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//         json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_SOUTH",
//                       "params": {
//                         "disperser": ${d.tag.toScreamingSnakeCase}
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//       )
//     }
//   }
//
//   test("gmosN_fpu") {
//     Enumerated[GmosNorthFpu].all.traverse { d =>
//       query(
//         s"""
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               centimetersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: POINT_THREE,
//               cloudExtinction: POINT_FIVE,
//               skyBackground: DARK,
//               waterVapor: DRY,
//               elevationRange: {
//                 airmassRange: {
//                   min: 1,
//                   max: 2
//                 }
//               }
//             },
//             modes: [{
//               gmosN: {
//                 filter: G_PRIME,
//                 fpu: ${d.tag.toScreamingSnakeCase}
//                 disperser: B1200_G5301
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   params {
//                     ... on GmosNITCParams {
//                       fpu
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//         json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_NORTH",
//                       "params": {
//                         "fpu": ${d.tag.toScreamingSnakeCase}
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//       )
//     }
//   }
//
//   test("gmosS_fpu") {
//     Enumerated[GmosSouthFpu].all.filter(_ =!= GmosSouthFpu.Bhros).traverse { d =>
//       query(
//         s"""
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               centimetersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: POINT_THREE,
//               cloudExtinction: POINT_FIVE,
//               skyBackground: DARK,
//               waterVapor: DRY,
//               elevationRange: {
//                 airmassRange: {
//                   min: 1,
//                   max: 2
//                 }
//               }
//             },
//             modes: [{
//               gmosS: {
//                 filter: G_PRIME,
//                 fpu: ${d.tag.toScreamingSnakeCase}
//                 disperser: B1200_G5321
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   params {
//                     ... on GmosSITCParams {
//                       fpu
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//         json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_SOUTH",
//                       "params": {
//                         "fpu": ${d.tag.toScreamingSnakeCase}
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//       )
//     }
//   }
//
//   test("gmosN_filter") {
//     Enumerated[GmosNorthFilter].all.traverse { d =>
//       query(
//         s"""
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               centimetersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: POINT_THREE,
//               cloudExtinction: POINT_FIVE,
//               skyBackground: DARK,
//               waterVapor: DRY,
//               elevationRange: {
//                 airmassRange: {
//                   min: 1,
//                   max: 2
//                 }
//               }
//             },
//             modes: [{
//               gmosN: {
//                 filter: ${d.tag.toScreamingSnakeCase}
//                 fpu: LONG_SLIT_0_25,
//                 disperser: B1200_G5301
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   params {
//                     ... on GmosNITCParams {
//                       filter
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//         json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_NORTH",
//                       "params": {
//                         "filter": ${d.tag.toScreamingSnakeCase}
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//       )
//     }
//   }
//
//   test("gmosS_filter") {
//     Enumerated[GmosSouthFilter].all.traverse { d =>
//       println(d)
//       query(
//         s"""
//         query {
//           spectroscopy(input: {
//             wavelength: {
//               nanometers: 60,
//             },
//             radialVelocity: {
//               centimetersPerSecond: 1000
//             },
//             signalToNoise: 2,
//             spatialProfile: {
//               sourceType: POINT_SOURCE
//             },
//             spectralDistribution: {
//               blackBody: {
//                 temperature: 50.1
//               }
//             },
//             magnitude: {
//               band: AP,
//               value: 5,
//               error: 1.2,
//               system: JY
//             },
//             constraints: {
//               imageQuality: POINT_THREE,
//               cloudExtinction: POINT_FIVE,
//               skyBackground: DARK,
//               waterVapor: DRY,
//               elevationRange: {
//                 airmassRange: {
//                   min: 1,
//                   max: 2
//                 }
//               }
//             },
//             modes: [{
//               gmosS: {
//                 filter: ${d.tag.toScreamingSnakeCase}
//                 fpu: LONG_SLIT_0_25,
//                 disperser: B1200_G5321
//               }
//             }
//             ]
//           }) {
//             results {
//                 mode {
//                   instrument
//                   params {
//                     ... on GmosSITCParams {
//                       filter
//                     }
//                   }
//                   wavelength {
//                     nanometers
//                   }
//                 }
//                 itc {
//                   ... on ItcSuccess {
//                     exposures
//                     exposureTime {
//                       seconds
//                     }
//                   }
//                 }
//             }
//           }
//         }
//         """,
//         json"""
//         {
//           "data": {
//             "spectroscopy" : [
//               {
//                 "results" : [
//                   {
//                     "mode" : {
//                       "instrument" : "GMOS_SOUTH",
//                       "params": {
//                         "filter": ${d.tag.toScreamingSnakeCase}
//                       },
//                       "wavelength" : {
//                         "nanometers" : 60.00
//                       }
//                     },
//                     "itc" : {
//                       "exposures" : 10,
//                       "exposureTime" : {
//                         "seconds" : 1
//                       }
//                     }
//                   }
//                 ]
//               }
//             ]
//           }
//         }
//         """
//       )
//     }
//   }
}
