package net.scalax.fsn.slick.model

import org.xarcher.cpoi.CellData
import slick.dbio.DBIO

case class PoiOut(properties: List[SelectProperty], data: SlickParam => DBIO[(List[Map[String, CellData[_]]], Int)])