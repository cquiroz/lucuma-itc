// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.effect._
import io.circe.Json
import lucuma.itc.Itc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import natchez.Trace.Implicits.noop
import org.http4s._
import org.http4s.circe._
import org.http4s.syntax.all._

import scala.concurrent.duration._

trait GraphQLSuite extends munit.CatsEffectSuite {
  val itc = new Itc[IO] {
    def calculate(
      targetProfile: TargetProfile,
      observingMode: ObservingMode,
      signalToNoise: Int
    ): IO[Itc.Result] =
      IO.pure(
        Itc.Result.Success(1.seconds, 10, 10)
      )
  }

  val service: IO[HttpRoutes[IO]] =
    ItcMapping[IO](itc).map(m => ItcService.routes[IO](ItcService.service[IO](m)))
  val itcFixture                  = ResourceSuiteLocalFixture(
    "itc",
    Resource.make(service)(_ => IO.unit)
  )

  override def munitFixtures = List(itcFixture)

  def query(query: String, expected: Json): IO[Unit] =
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(
          Request(method = Method.POST, uri = uri"/itc")
            .withEntity(Json.obj("query" -> Json.fromString(query)))
        )
      }
      .flatMap(_.as[Json])
      .assertEquals(expected)
}
