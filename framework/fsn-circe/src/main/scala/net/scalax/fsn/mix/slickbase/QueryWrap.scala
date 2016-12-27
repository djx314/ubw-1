package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.core.{FColumn, FPile}
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl

import scala.concurrent.ExecutionContext

case class PileListQueryWrap(
  columns: List[FPile[Option]],
  listQueryBind: SlickQueryBindImpl
)(implicit val ec: ExecutionContext)

case class ListQueryWrap(
  columns: List[FColumn],
  listQueryBind: SlickQueryBindImpl
)(implicit val ec: ExecutionContext)

case class FQueryWrap(
  binds: List[(Any, SlickQueryBindImpl)],
  columns: List[FColumn]
)
/*
case class QueryWrap(
  binds: List[(Any, SlickQueryBindImpl)],
  listQueryWrap: ListQueryWrap
)*/