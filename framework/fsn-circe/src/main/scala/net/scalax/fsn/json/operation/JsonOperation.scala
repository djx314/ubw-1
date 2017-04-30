package net.scalax.fsn.json.operation

import io.circe.Json
import io.circe.syntax._
import net.scalax.fsn.common.atomic.{ DefaultValue, FProperty }
import net.scalax.fsn.core._
import net.scalax.fsn.json.atomic.{ JsonReader, JsonWriter }
import shapeless._

object JsonOperation {

  val readGen = FPile.transformTreeList {
    new FAtomicQuery(_) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: HNil)
        .mapToOption {
          case (jsonReader :: property :: defaultOpt :: HNil, data) =>
            val tran: Map[String, Json] => Option[path.DataType] = { sourceData: Map[String, Json] =>
              sourceData.get(property.proName) match {
                case Some(json) =>
                  json.as[jsonReader.JsonType](jsonReader.reader) match {
                    case Right(data) =>
                      Option(jsonReader.convert(data))
                    case _ =>
                      val ifEmptyData = data.fold(defaultOpt.map(_.value))(Option(_))
                      if (ifEmptyData.isEmpty) {
                        throw new Exception(s"字段 ${property.proName} 的值不能被正确转换")
                      }
                      ifEmptyData
                  }
                case None =>
                  val ifEmptyData = data.fold(defaultOpt.map(_.value))(Option(_))
                  if (ifEmptyData.isEmpty) {
                    throw new Exception(s"字段 ${property.proName} 未被定义")
                  }
                  ifEmptyData
              }
            }

            tran: (Map[String, Json] => Option[Any])
        }
    }.aa
  } { readlerList =>
    { sourceData: Map[String, Json] =>
      readlerList.map(_.apply(sourceData))
    }
  }

  val unfullReadGen = FPile.transformTreeList {
    new FAtomicQuery(_) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: HNil)
        .mapToOption {
          case (jsonReader :: property :: defaultOpt :: HNil, data) =>
            val tran: Map[String, Json] => Option[path.DataType] = { sourceData: Map[String, Json] =>
              sourceData.get(property.proName) match {
                case Some(json) =>
                  json.as[jsonReader.JsonType](jsonReader.reader) match {
                    case Right(data) =>
                      Option(jsonReader.convert(data))
                    case _ =>
                      val ifEmptyData = data.fold(defaultOpt.map(_.value))(Option(_))
                      /*if (ifEmptyData.isEmpty) {
                      throw new Exception(s"字段 ${ property.proName } 的值不能被正确转换")
                    }*/
                      ifEmptyData
                  }
                case None =>
                  val ifEmptyData = data.fold(defaultOpt.map(_.value))(Option(_))
                  /*if (ifEmptyData.isEmpty) {
                  throw new Exception(s"字段 ${ property.proName } 未被定义")
                }*/
                  ifEmptyData
              }
            }

            tran: (Map[String, Json] => Option[Any])
        }
    }.aa
  } { readlerList =>
    { sourceData: Map[String, Json] =>
      readlerList.map(_.apply(sourceData))
    }
  }

  def writeGen = FPile.transformTreeList {
    new FAtomicQuery(_) {
      val aa = withRep(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: HNil)
        .mapToOption {
          case (jsonWriter :: property :: defaultOpt :: HNil, data) => {
            val exportData = data.fold(defaultOpt.map(_.value))(Option(_))
            val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            property.proName -> jsonWriter.convert(eachColumnData).asJson(jsonWriter.writer)
          }
        }
    }.aa
  } { jsonTupleList =>
    jsonTupleList.toMap: Map[String, Json]
  }

  def unSafewriteGen = FPile.transformTreeList {
    new FAtomicQuery(_) {
      val aa = withRep(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: HNil)
        .mapToOption {
          case (jsonWriter :: property :: defaultOpt :: HNil, data) => {
            val exportData = data.fold(defaultOpt.map(_.value))(Option(_))
            //val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            implicit val writerJ = jsonWriter.writer
            exportData.map(s => property.proName -> jsonWriter.convert(s).asJson)
          }
        }
    }.aa
  } { jsonTupleList =>
    jsonTupleList.collect { case Some(s) => s }.toMap: Map[String, Json]
  }

}