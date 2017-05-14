package net.scalax.fsn.slick.helpers

import slick.ast.ScalaBaseType
import slick.lifted._

trait LikeableColumnGen[S] {

  type BooleanTypeRep <: Rep[_]

  def dataToCondition(rep: S, str: String): BooleanTypeRep
  val wt: CanBeQueryCondition[BooleanTypeRep]

}

object LikeableColumnGen {

  object ExtensionMethodConversionsHelper extends ExtensionMethodConversions

  import ExtensionMethodConversionsHelper._

  implicit val strLikeableColumnGenImplicit: LikeableColumnGen[Rep[String]] = new LikeableColumnGen[Rep[String]] {
    override type BooleanTypeRep = Rep[Boolean]
    override def dataToCondition(rep: Rep[String], str: String): BooleanTypeRep = {
      implicit val strTypeImplicit = ScalaBaseType.stringType
      rep.like(LiteralColumn(str))(OptionMapper2.getOptionMapper2TT)
    }
    override val wt: CanBeQueryCondition[BooleanTypeRep] = CanBeQueryCondition.BooleanColumnCanBeQueryCondition
  }

  implicit val strOptLikeableColumnGenImplicit: LikeableColumnGen[Rep[Option[String]]] = new LikeableColumnGen[Rep[Option[String]]] {
    override type BooleanTypeRep = Rep[Option[Boolean]]
    override def dataToCondition(rep: Rep[Option[String]], str: String): BooleanTypeRep = {
      implicit val strTypeImplicit = ScalaBaseType.stringType
      rep.like(LiteralColumn(str))(OptionMapper2.getOptionMapper2OT)
    }
    override val wt: CanBeQueryCondition[BooleanTypeRep] = CanBeQueryCondition.BooleanOptionColumnCanBeQueryCondition
  }

}