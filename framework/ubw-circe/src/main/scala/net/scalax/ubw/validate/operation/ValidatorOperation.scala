package net.scalax.ubw.json.operation

import net.scalax.ubw.common.atomic.{ FDescribe, FProperty }
import net.scalax.ubw.core._
import net.scalax.ubw.validate.atomic.{ ErrorMessage, ValidatorF }
import shapeless._

import scala.concurrent.{ ExecutionContext, Future }

object ValidatorOperation extends AtomicValueHelper {

  def readValidator(implicit ec: ExecutionContext): AtomicPath => QueryTranform[Future[List[ErrorMessage]]] = {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[FProperty] :: needAtomicList[ValidatorF] :: needAtomicOpt[FDescribe] :: FANil)
        .mapTo {
          case (property :: validateFList :: describeOpt :: HNil, data) =>
            val dataOpt = data match {
              case FSomeValue(s) => Option(s)
              case AtomicValueImpl.Zero() => Option.empty[data.DataType]
            }

            val validateResultFList = validateFList.map { validateF =>
              val par = validateF.validateF(property.proName, describeOpt.map(_.describe).getOrElse(property.proName))
              val isDefinedAt = par.isDefinedAt(dataOpt)
              val messageResult: Future[Option[ErrorMessage]] = if (isDefinedAt) {
                par.apply(dataOpt)
              } else {
                Future successful Option.empty[ErrorMessage]
              }
              messageResult /*.map { s =>
                (if (s.isDefined) {
                  emptyValue[data.DataType] -> s.toList
                } else {
                  data -> s.toList
                }): (AtomicValue, List[ErrorMessage])
              }*/
            }
            Future.sequence(validateResultFList).map { s =>
              val validateResult = s.collect { case Some(s) => s }
              (if (validateResult.isEmpty) {
                validateResult
              } else {
                validateResult
              }): List[ErrorMessage]
            }
        }
    }.aa
  }

}