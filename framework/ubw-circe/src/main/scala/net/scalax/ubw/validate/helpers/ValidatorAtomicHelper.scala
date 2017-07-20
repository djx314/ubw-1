package net.scalax.ubw.validate.helpers

import net.scalax.fsn.json.operation.FAtomicHelper
import net.scalax.ubw.validate.atomic.{ ErrorMessage, Validator }

trait ValidatorAtomicHelper[E] extends FAtomicHelper[E] {

  trait NestEmptyUnWrap[S, T] {
    def unWrap(s: S): Option[T]
  }

  implicit def commonStrOptUnWrapImplicit[T]: NestEmptyUnWrap[Option[T], T] = {
    new NestEmptyUnWrap[Option[T], T] {
      override def unWrap(s: Option[T]): Option[T] = s
    }
  }

  implicit def nestatStrOptUnWrapImplicit[T, R](implicit sub: NestEmptyUnWrap[R, T]): NestEmptyUnWrap[Option[R], T] = {
    new NestEmptyUnWrap[Option[R], T] {
      override def unWrap(s: Option[R]): Option[T] = s.flatMap(t => sub.unWrap(t))
    }
  }

  def notEmptyString(implicit strOptUnWrap: NestEmptyUnWrap[Option[E], String]) = path.appendAtomic(new Validator[E] {
    override def validate(proName: String): PartialFunction[Option[E], Option[ErrorMessage]] = {
      case s =>
        lazy val emptyErrMsg = Option(ErrorMessage(s"${proName}不能为空"))
        strOptUnWrap.unWrap(s) match {
          case Some(s) if !s.isEmpty =>
            None
          case Some(_) =>
            emptyErrMsg
          case None =>
            emptyErrMsg
        }
    }
  })

  def specNotNull[T](implicit optUnWrap: NestEmptyUnWrap[Option[E], T]) = path.appendAtomic(new Validator[E] {
    override def validate(proName: String): PartialFunction[Option[E], Option[ErrorMessage]] = {
      case s =>
        lazy val emptyErrMsg = Option(ErrorMessage(s"${proName}不能为空"))
        optUnWrap.unWrap(s) match {
          case Some(_) =>
            None
          case None =>
            emptyErrMsg
        }
    }
  })

  def notNull = path.appendAtomic(new Validator[E] {
    override def validate(proName: String): PartialFunction[Option[E], Option[ErrorMessage]] = {
      case s =>
        lazy val emptyErrMsg = Option(ErrorMessage(s"${proName}不能为空"))
        s.fold(emptyErrMsg) { s => None }
    }
  })

  def isInt(implicit strOptUnWrap: NestEmptyUnWrap[Option[E], String]) = path.appendAtomic(new Validator[E] {
    override def validate(proName: String): PartialFunction[Option[E], Option[ErrorMessage]] = {
      case s @ (Some(_)) =>
        lazy val emptyErrMsg = Option(ErrorMessage(s"${proName}不是数字"))
        strOptUnWrap.unWrap(s).filterNot(_.isEmpty).flatMap { t =>
          try {
            Integer.valueOf(t)
            None
          } catch {
            case e: NumberFormatException =>
              emptyErrMsg
          }
        }
    }
  })

  def isNumber(implicit strOptUnWrap: NestEmptyUnWrap[Option[E], String]) = path.appendAtomic(new Validator[E] {
    override def validate(proName: String): PartialFunction[Option[E], Option[ErrorMessage]] = {
      case s @ (Some(_)) =>
        lazy val emptyErrMsg = Option(ErrorMessage(s"${proName}不是数字"))
        strOptUnWrap.unWrap(s).filterNot(_.isEmpty).flatMap { t =>
          try {
            java.lang.Double.valueOf(t)
            None
          } catch {
            case e: NumberFormatException =>
              emptyErrMsg
          }
        }
    }
  })

}