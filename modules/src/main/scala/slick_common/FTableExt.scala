package net.scalax.fsn.slick_common

import slick.ast._
import slick.lifted._
import slick.profile.RelationalProfile

trait FRepExt {

  val owner: RelationalProfile#Table[_]

}

trait FTable {
  self: RelationalProfile#Table[_] =>

  val _tableName: String

  def fColumn[C](n: String, options: ColumnOption[C]*)(implicit tt: TypedType[C]): Rep[C] with FRepExt = {

    if(tt == null) throw new NullPointerException(
      "implicit TypedType[C] for column[C] is null. "+
        "This may be an initialization order problem. "+
        "When using a MappedColumnType, you may want to change it from a val to a lazy val or def.")

    new Rep.TypedRep[C] with FRepExt {

      override def toNode =
        Select((tableTag match {
          case r: RefTag => r.path
          case _ => tableNode
        }), FieldSymbol(n)(options, tt)) :@ tt

      override def toString = (tableTag match {
        case r: RefTag => "(" + _tableName + " " + r.path + ")"
        case _ => _tableName
      }) + "." + n

      override val owner = self

    }
  }

}