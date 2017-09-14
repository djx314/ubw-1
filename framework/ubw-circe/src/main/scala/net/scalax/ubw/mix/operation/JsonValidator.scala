package net.scalax.ubw.mix.operation

import net.scalax.ubw.core._
import net.scalax.ubw.json.operation.{ JsonOperation, ValidatorOperation }
import io.circe.JsonObject
import net.scalax.ubw.validate.atomic.ErrorMessage

import scala.concurrent.{ ExecutionContext, Future }

object JsonValidator extends PilesGenHelper {

  def validate(jsonObj: JsonObject, piles: List[Pile])(implicit ec: ExecutionContext): Future[Either[List[ErrorMessage], JsonObject => JsonObject]] = {
    JsonOperation.jsonObjectReadGen.next(JsonOperation.jsonObjectWriteGen.withFilter(ValidatorOperation.readValidator)).result(piles) match {
      case Left(e) =>
        e.printStackTrace()
        throw e
      case Right(result1) =>
        val result = result1.apply(jsonObj)
        result._1.map { errors =>
          if (errors.isEmpty) {
            Right(result._2)
          } else {
            Left(errors)
          }
        }
    }
  }

}