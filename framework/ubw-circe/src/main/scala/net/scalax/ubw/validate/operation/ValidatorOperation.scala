package net.scalax.fsn.json.operation

import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.core._
import net.scalax.ubw.core.PileFilter
import net.scalax.ubw.validate.atomic.{ ErrorMessage, ValidatorF }
import shapeless._
import cats.instances.future._
import cats._

import scala.concurrent.{ ExecutionContext, Future }

object ValidatorOperation extends FAtomicValueHelper {

  def readValidator(implicit ec: ExecutionContext): PileFilter[List[ErrorMessage], Future] = PileFilter {
    new FAtomicQuery(_) {
      val aa = withRep(needAtomic[FProperty] :: needAtomic[ValidatorF] :: FANil)
        .mapTo {
          case (property :: validateF :: HNil, data) =>
            val par = validateF.validateF(property.proName)
            val dataOpt = data match {
              case FSomeValue(s) => Option(s)
              case FAtomicValueImpl.Zero() => Option.empty[data.DataType]
            }
            val isDefinedAt = par.isDefinedAt(dataOpt)
            val messageResult: Future[Option[ErrorMessage]] = if (isDefinedAt) {
              par.apply(dataOpt)
            } else {
              Future successful Option.empty[ErrorMessage]
            }
            messageResult.map { s =>
              (if (s.isEmpty) {
                emptyValue[data.DataType] -> s.toList
              } else {
                data -> s.toList
              }): (FAtomicValue, List[ErrorMessage])
            }
        }
    }.aa
  }(implicitly[Monad[Future]], new Semigroup[List[ErrorMessage]] {
    override def combine(x: List[ErrorMessage], y: List[ErrorMessage]): List[ErrorMessage] = {
      x ::: y
    }
  })

}