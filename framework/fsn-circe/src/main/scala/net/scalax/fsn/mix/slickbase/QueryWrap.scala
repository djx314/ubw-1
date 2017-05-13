package net.scalax.fsn.mix.slickbase

import net.scalax.fsn.core.FPile1111
import net.scalax.fsn.slick.helpers.SlickQueryBindImpl

import scala.concurrent.ExecutionContext

/*case class PileListQueryWrap(
  columns: List[FPile[Option]],
  listQueryBind: SlickQueryBindImpl
)(implicit val ec: ExecutionContext)*/

case class PileListQueryWrap1111(
  columns: List[FPile1111],
  listQueryBind: SlickQueryBindImpl
)(implicit val ec: ExecutionContext)

case class FQueryWrap(
  binds: List[(Any, SlickQueryBindImpl)],
  columns: List[FPile1111]
)
