package aaaa

import slick.ast.BaseTypedType
import slick.lifted._

import scala.language.existentials
import scala.language.implicitConversions
import scala.language.higherKinds

trait FilterColumnGen[S] {

  type BooleanTypeRep <: Rep[_]

  val dataToCondition: S => BooleanTypeRep
  val wt: CanBeQueryCondition[BooleanTypeRep]

}

trait FilterBaseConvert {

  type SourceRep
  type FilterType
  type BooleanType

  val typeGen: SourceRep <:< Rep[FilterType]
  //val dataGen: FilterType
  val baseType: BaseTypedType[FilterType]

  val colImplicit: Rep[FilterType] => BaseColumnExtensionMethods[FilterType]

  val om: OptionMapperDSL.arg[FilterType, FilterType]#arg[FilterType, FilterType]#to[Boolean, BooleanType]

  val wt: CanBeQueryCondition[Rep[BooleanType]]

}

trait FilterBaseConvertGen[S, T] extends FilterBaseConvert {

  override type SourceRep = S
  override type FilterType = T

}

trait FilterOptConvert {

  type SourceRep
  type NoneOptType
  type BooleanType

  val typeGen: SourceRep <:< Rep[Option[NoneOptType]]

  val baseType: BaseTypedType[NoneOptType]

  val colImplicit: Rep[Option[NoneOptType]] => OptionColumnExtensionMethods[NoneOptType]

  val om: OptionMapperDSL.arg[NoneOptType, Option[NoneOptType]]#arg[NoneOptType, Option[NoneOptType]]#to[Boolean, BooleanType]

  val wt: CanBeQueryCondition[Rep[BooleanType]]

}

trait FilterOptConvertGen[S, T] extends FilterOptConvert {

  override type SourceRep = S
  override type NoneOptType = T

}

trait FilterWrapper1111[S, D] {

  type BooleanTypeRep <: Rep[_]

  val dataToCondition: S => D => BooleanTypeRep
  val wt: CanBeQueryCondition[BooleanTypeRep]

}
/*trait FilterJsonGen[S] {

  type BooleanTypeRep <: Rep[_]

  val dataToCondition: S => Map[String, Json] => BooleanTypeRep
  val wt: CanBeQueryCondition[BooleanTypeRep]

}*/
object FilterWrapper1111 {
  def fromIBaseConvert[S, T](convert1: FilterBaseConvertGen[S, T]): aaaa.FilterWrapper1111[S, T] = {
    new aaaa.FilterWrapper1111[convert1.SourceRep, convert1.FilterType] {
      override type BooleanTypeRep = Rep[convert1.BooleanType]
      override val dataToCondition = { baseRep1: convert1.SourceRep => { data: convert1.FilterType =>
          convert1.colImplicit(convert1.typeGen(baseRep1)).===(LiteralColumn(data)(convert1.baseType))(convert1.om)
      } }
      override val wt = convert1.wt
    }
  }

  def fromIOptConvert[S, T](convert1: FilterOptConvertGen[S, T]): aaaa.FilterWrapper1111[S, Option[T]] = {
     new aaaa.FilterWrapper1111[convert1.SourceRep, Option[convert1.NoneOptType]] {
      override type BooleanTypeRep = Rep[convert1.BooleanType]
      override val dataToCondition = { baseRep1: convert1.SourceRep => { data: Option[convert1.NoneOptType] =>
        convert1.colImplicit(convert1.typeGen(baseRep1)).===(LiteralColumn(data)(convert1.baseType.optionType))(convert1.om)
      } }
      override val wt = convert1.wt
    }
  }

}

trait FilterRepImplicit1111 {

  implicit def baseWrapperConvert[A, B, C](
    implicit
    cv: A <:< Rep[C],
    //decoder: Decoder[C],
    baseTypedType1: BaseTypedType[C],
    colImplicit1: Rep[C] => BaseColumnExtensionMethods[C],
    om1: OptionMapperDSL.arg[C, C]#arg[C, C]#to[Boolean, B],
    wt1: CanBeQueryCondition[Rep[B]]
  ): aaaa.FilterWrapper1111[A, C] = {
    val convert = new FilterBaseConvertGen[A, C] {
      override type BooleanType = B
      override val typeGen = cv
      override val baseType = baseTypedType1
      override val colImplicit = colImplicit1
      override val om = om1
      override val wt = wt1
    }
    FilterWrapper1111.fromIBaseConvert(convert)
  }

  implicit def baseOptWrapperConvert[A, B, C](
    implicit
    cv: A <:< Rep[Option[C]],
    //decoder: Decoder[Option[C]],
    baseTypedType1: BaseTypedType[C],
    colImplicit1: Rep[Option[C]] => OptionColumnExtensionMethods[C],
    om1: OptionMapperDSL.arg[C, Option[C]]#arg[C, Option[C]]#to[Boolean, B],
    wt1: CanBeQueryCondition[Rep[B]]
  ): aaaa.FilterWrapper1111[A, Option[C]] = {
    val convert = new FilterOptConvertGen[A, C] {
      override type BooleanType = B
      override val typeGen = cv
      override val baseType = baseTypedType1
      override val colImplicit = colImplicit1
      override val om = om1
      override val wt = wt1
    }
    FilterWrapper1111.fromIOptConvert(convert)
  }

}