// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// // Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// // For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
//
// package lucuma.odb.api.schema
//
// import cats.Parallel
// import cats.syntax.all._
// import lucuma.core.math.Redshift
// import lucuma.odb.api.repo.ItcRepo
// import lucuma.odb.search._
// import lucuma.odb.api.model.syntax.validatedinput._
// import lucuma.odb.api.model.ValidatedInput
// import lucuma.odb.itc._
// import sangria.schema._
// import cats.effect.std.Dispatcher
// import cats.MonadError
// import lucuma.odb.api.model.SpectralDistribution
// import lucuma.odb.api.model.enum.StellarLibrarySpectrum
//
// trait ConfigurationQuery {
//
//   import context._
//   import ConfigurationAlternativesSchema._
//
//   // TBD Add return type and hook it to the basic case algorithm
//   def spectroscopy[F[_]: Dispatcher: MonadError[*[_], Throwable]: Parallel: Itc]: Field[ItcRepo[F], Unit] =
//     Field(
//       name        = "spectroscopy",
//       fieldType   = SpectroscopyResultsType[F],
//       description = None,
//       arguments   = List(ArgumentConfigurationAlternativesModelSearch),
//       resolve     = c => c.unsafeToFuture {
//         val arg = c.arg(ArgumentConfigurationAlternativesModelSearch)
//         val result  = (
//           arg.wavelength.toWavelength("wavelength"),
//           arg.simultaneousCoverage.toWavelength("simultaneousCoverage"),
//           arg.resolution.validNec,
//           arg.spatialProfile.toSpatialProfile,
//           arg.spectralDistribution.validNec,
//           arg.magnitude.toMagnitude,
//           arg.redshift.validNec,
//           ).traverseN {(wavelength, simultaneousCoverage, resolution, spatialProfile, spectralDistribution, magnitude, redshift) =>
//             println("cos:")
//             val constraints = Constraints.Spectroscopy(wavelength, simultaneousCoverage, resolution)
//             val targetProfile = TargetProfile(spatialProfile, SpectralDistribution.Library(StellarLibrarySpectrum.A0V.asLeft), magnitude, Redshift(redshift))
//             val r = Search.spectroscopy[F](constraints, targetProfile, arg.signalToNoise)
//             println(r)
//             r
//           }
//           println(result)
//         result.flatMap(_.liftTo[F])//map(_.value)//Or(???))
//       }
//     )
//
//   def allFields[F[_]: Dispatcher: Parallel: MonadError[*[_], Throwable]: Itc]: List[Field[ItcRepo[F], Unit]] = {
//     List(
//       spectroscopy
//     )
//   }
//
// }
//
// object ConfigurationQuery extends ConfigurationQuery
