// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import cats._
import cats.implicits._

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import edu.gemini.grackle.circe.CirceMapping

import Query._, Path._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.{ mkErrorResult, mkOneError }
import generic._, semiauto._
import io.circe.Decoder
import io.circe.Encoder

object ItcMapping extends CirceMapping[Id] with ComputeMapping[Id] {
 def beast(env: Cursor.Env): Id[Result[Beast]] =
   Result(base)

 val schema = schema"""
	type Query {
		beasts: Beast!
	}
  type Beast {
    id: Int
  }
 """

  val QueryType    = schema.ref("Query")
  val BeastType = schema.ref("Beast")

  final case class Beast(id: Int)
  object Beast {
    import io.circe.generic.semiauto._
    // implicit val cursorBuilder: CursorBuilder[Beast] =
    //   // deriveCursorBuilder[Beast](BeastType)
    //   deriveObjectCursorBuilder[Beast](BeastType)
    // implicit val decoderBeast: Decoder[Beast] = ???
    implicit val encoderBeast: Encoder[Beast] =
      deriveEncoder[Beast]
  }
  val base: Beast = new Beast(0)
 val typeMappings =
  List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        ComputeRoot("beasts", BeastType, beast)
      )
    ),
    // LeafMapping[Beast](BeastType),
  )
}
