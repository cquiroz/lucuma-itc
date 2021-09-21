// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

/**
 * The main "repository" for the API server.  It is simply a collection of
 * repositories for the top-level types.
 */
trait ItcRepo[F[_]] {

  // def tables: Ref[F, Tables]
  //
  // def eventService: EventService[F]
  //
  // def atom: AtomRepo[F]
  //
  // def executionEvent: ExecutionEventRepo[F]
  //
  // def observation: ObservationRepo[F]
  //
  // def program: ProgramRepo[F]
  //
  // def step: StepRepo[F]

}

object ItcRepo {

  /**
   * Creates an empty ODB repository backed by a `Ref` containing `Tables`.
   */
  def create[F[_]: cats.Applicative]: F[ItcRepo[F]] =
    cats.Applicative[F].pure(new ItcRepo[F] {})
}
