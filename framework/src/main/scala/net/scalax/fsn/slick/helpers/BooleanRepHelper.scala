package net.scalax.fsn.slick.helpers

import scala.language.implicitConversions
import slick.ast.TypedType
import slick.lifted._

import scala.annotation.tailrec

case class RichBRep[P](rep: Option[Rep[P]]) {

  def &&&[P2, R](rRep2: RichBRep[P2])
  (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    (rep -> rRep2.rep) match {
      case (Some(subRep1), Some(subRep2)) =>
        RichBRep(Option(new BooleanColumnExtensionMethods(subRep1).&&(subRep2)(om)))
      case (None, Some(subRep2)) =>
        RichBRep(Option(convertP2(subRep2)))
      case (Some(subRep1), None) =>
        RichBRep(Option(convertP(subRep1)))
      case _ =>
        RichBRep(None)
    }
  }

  def |||[P2, R](rRep2: RichBRep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    (rep -> rRep2.rep) match {
      case (Some(subRep1), Some(subRep2)) =>
        RichBRep(Option(new BooleanColumnExtensionMethods(subRep1).||(subRep2)(om)))
      case (None, Some(subRep2)) =>
        RichBRep(Option(convertP2(subRep2)))
      case (Some(subRep1), None) =>
        RichBRep(Option(convertP(subRep1)))
      case _ =>
        RichBRep(None)
    }
  }

  def &&&[P2, R](when: Boolean, rRep2: => Rep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    if (when) {
      &&&(RichBRep(Option(rRep2)))
    } else {
      &&&(RichBRep.empty[P2])
    }
  }

  def |||[P2, R](when: Boolean, rRep2: => Rep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    if (when) {
      |||(RichBRep(Option(rRep2)))
    } else {
      |||(RichBRep.empty[P2])
    }
  }

  def &&&[P2, R](rRep2: Option[Rep[P2]])
    (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    &&&(RichBRep(rRep2))
  }

  def |||[P2, R](rRep2: Option[Rep[P2]])
    (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    |||(RichBRep(rRep2))
  }

  def &&&[P2, R](rRep2: Rep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    &&&(RichBRep(Option(rRep2)))
  }

  def |||[P2, R](rRep2: Rep[P2])
                (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    |||(RichBRep(Option(rRep2)))
  }

  def ||=>[P2](rRep2: Rep[P2])
                 (implicit convertP: Rep[P] => Rep[Option[Boolean]], convertP2: Rep[P2] => Rep[Option[Boolean]]): Rep[Option[Boolean]] = {
    rep.map(convertP(_)).getOrElse(convertP2(rRep2))
  }

  def result(implicit tt: TypedType[Boolean], convert: Rep[P] => Rep[Option[Boolean]], columnMethods: Rep[Boolean] => BaseColumnExtensionMethods[Boolean]): Rep[Option[Boolean]] = {
    ||=>(LiteralColumn(true)(tt))(convert, (s: Rep[Boolean]) => columnMethods(s).?)
  }

}

object RichBRep {
  def empty[P] = RichBRep(Option.empty[Rep[P]])
}

trait BooleanRepHelper {

  @inline implicit def booleanRepToRichRep[T](rep: Rep[T]): RichBRep[T] = RichBRep(Option(rep))

  @inline implicit def booleanNeedRepToRichRep[T](plus: (Boolean, Rep[T])): RichBRep[T] = if (plus._1) {
    RichBRep(Option(plus._2))
  } else {
    RichBRep.empty[T]
  }

  @inline implicit def booleanToOptionBooleanRep(baseRep: Rep[Boolean])(implicit columnMethods: Rep[Boolean] => BaseColumnExtensionMethods[Boolean]): Rep[Option[Boolean]] = columnMethods(baseRep).?

}

object BooleanRepHelper extends BooleanRepHelper

object TypeHelpers {

  import scala.reflect.runtime.universe._

  @tailrec
  def unwrapWeakTypeTag(weak: Type): Type = {
    implicitly[WeakTypeTag[Option[String]]].tpe match {
      case TypeRef(c, d, args1) =>
        weak match {
          case TypeRef(a, b, args) =>
            if (a == c && b == d) {
              unwrapWeakTypeTag(args.head)
            } else {
              weak
            }
        }
    }

  }

}