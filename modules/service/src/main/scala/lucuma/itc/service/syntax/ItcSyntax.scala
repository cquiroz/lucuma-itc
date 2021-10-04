// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import cats.data._
import monocle.std.these._
import monocle.Focus
import edu.gemini.grackle.Query.Environment
import edu.gemini.grackle.Problem

trait ItcSyntax {

  implicit class MoreOptionOps[A](self: Option[A]) {
    def toRightIorNec[E](e: => E): Ior[NonEmptyChain[E], A] =
      self match {
        case Some(a) => Ior.Right(a)
        case None    => Ior.Left(NonEmptyChain.of(e))
      }
  }

  implicit class MoreIdOps[A](self: A) {

    def leftIorNec[B]: Ior[NonEmptyChain[A], B] =
      Ior.Left(NonEmptyChain.of(self))
  }

  implicit class IorOps[A](self: IorNec[Problem, A]) {
    def addProblem(problem: String): Ior[NonEmptyChain[Problem], A] =
      self.addLeft(NonEmptyChain.of(Problem(problem)))
  }

  implicit class StringOps(val self: String) {
    def fromScreamingSnakeCase: String =
      self.split("_").map(_.toLowerCase.capitalize).mkString("")
  }
  def cursorEnv[A] = theseRight[A, Environment].andThen(Focus[Environment](_.env))
  def cursorEnvAdd[A, B](key: String, value: B): Ior[A, Environment] => Ior[A, Environment] =
    cursorEnv[A].modify(_.add((key, value)))

}

object all extends ItcSyntax
