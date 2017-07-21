package net.scalax.ubw.validate.atomic

import net.scalax.fsn.core.FAtomic

import scala.concurrent.Future

trait ValidatorF[E] extends FAtomic[E] {

  type DataType = E
  def validateF(proName: String): PartialFunction[Option[DataType], Future[Option[ErrorMessage]]]
}

trait Validator[E] extends ValidatorF[E] {

  override type DataType = E

  def validate(proName: String): PartialFunction[Option[DataType], Option[ErrorMessage]]

  override def validateF(proName: String): PartialFunction[Option[DataType], Future[Option[ErrorMessage]]] = {
    {
      case data: Option[DataType] if validate(proName).isDefinedAt(data) =>
        Future successful validate(proName).apply(data)
    }
  }

}

case class ErrorMessage(mess: String, level: String)

object ErrorMessage {

  val errorType = "error"
  val warnType = "warn"

  def error(message: String): ErrorMessage = {
    ErrorMessage(message, errorType)
  }

  def warn(message: String): ErrorMessage = {
    ErrorMessage(message, warnType)
  }

}