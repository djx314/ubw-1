package net.scalax.ubw.slick.helpers

import slick.ast.BaseTypedType
import slick.lifted._

trait FilterColumnGen[S] {

  type BooleanTypeRep <: Rep[_]

  val dataToCondition: S => BooleanTypeRep
  val wt: CanBeQueryCondition[BooleanTypeRep]

}

trait FilterWrapper[S, D] {

  type BooleanTypeRep <: Rep[_]

  val dataToCondition: S => D => BooleanTypeRep
  val wt: CanBeQueryCondition[BooleanTypeRep]

}

trait FilterBaseConvert {

  type SourceRep
  type FilterType
  type BooleanType

  val typeGen: SourceRep <:< Rep[FilterType]

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

object FilterWrapper extends FilterRepImplicitHelper {
  def fromIBaseConvert[S, T](convert1: FilterBaseConvertGen[S, T]): FilterWrapper[S, T] = {
    new FilterWrapper[convert1.SourceRep, convert1.FilterType] {
      override type BooleanTypeRep = Rep[convert1.BooleanType]
      override val dataToCondition = { baseRep1: convert1.SourceRep =>
        { data: convert1.FilterType =>
          convert1.colImplicit(convert1.typeGen(baseRep1)).===(LiteralColumn(data)(convert1.baseType))(convert1.om)
        }
      }
      override val wt = convert1.wt
    }
  }

  def fromIOptConvert[S, T](convert1: FilterOptConvertGen[S, T]): FilterWrapper[S, Option[T]] = {
    new FilterWrapper[convert1.SourceRep, Option[convert1.NoneOptType]] {
      override type BooleanTypeRep = Rep[convert1.BooleanType]
      override val dataToCondition = { baseRep1: convert1.SourceRep =>
        { data: Option[convert1.NoneOptType] =>
          convert1.colImplicit(convert1.typeGen(baseRep1)).===(LiteralColumn(data)(convert1.baseType.optionType))(convert1.om)
        }
      }
      override val wt = convert1.wt
    }
  }

}

trait FilterRepImplicitHelper {

  implicit def baseWrapperConvert[A, B, C](
    implicit
    cv: A <:< Rep[C],
    baseTypedType1: BaseTypedType[C],
    colImplicit1: Rep[C] => BaseColumnExtensionMethods[C],
    om1: OptionMapperDSL.arg[C, C]#arg[C, C]#to[Boolean, B],
    wt1: CanBeQueryCondition[Rep[B]]
  ): FilterWrapper[A, C] = {
    val convert = new FilterBaseConvertGen[A, C] {
      override type BooleanType = B
      override val typeGen = cv
      override val baseType = baseTypedType1
      override val colImplicit = colImplicit1
      override val om = om1
      override val wt = wt1
    }
    FilterWrapper.fromIBaseConvert(convert)
  }

  implicit def baseOptWrapperConvert[A, B, C](
    implicit
    cv: A <:< Rep[Option[C]],
    baseTypedType1: BaseTypedType[C],
    colImplicit1: Rep[Option[C]] => OptionColumnExtensionMethods[C],
    om1: OptionMapperDSL.arg[C, Option[C]]#arg[C, Option[C]]#to[Boolean, B],
    wt1: CanBeQueryCondition[Rep[B]]
  ): FilterWrapper[A, Option[C]] = {
    val convert = new FilterOptConvertGen[A, C] {
      override type BooleanType = B
      override val typeGen = cv
      override val baseType = baseTypedType1
      override val colImplicit = colImplicit1
      override val om = om1
      override val wt = wt1
    }
    FilterWrapper.fromIOptConvert(convert)
  }

}