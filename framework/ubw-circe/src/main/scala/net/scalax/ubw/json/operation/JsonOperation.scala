package net.scalax.ubw.json.operation

import cats.Functor
import io.circe.{ Json, JsonObject }
import io.circe.syntax._
import net.scalax.ubw.common.atomic.{ DefaultValue, FProperty }
import net.scalax.ubw.core._
import net.scalax.ubw.json.atomic.{ JsonReader, JsonWriter }
import shapeless._

object JsonOperation extends AtomicValueHelper with PilesGenHelper {

  def unSafewriteGen: SingleInputChannel[Map[String, Json]] = DataPile.transformTree {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
        .mapTo {
          case (jsonWriter :: property :: defaultOpt :: HNil, data) => {
            val exportData = mergeDefault(defaultOpt, data)
            //val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            implicit val writerJ = jsonWriter.writer
            exportData.map(s => property.proName -> s.asJson)
          }
        }
    }.aa
  } { (jsonTupleList, atomicGen) =>
    jsonTupleList.collect { case Some(s) => s }.toMap: Map[String, Json]
  }

  type V[A] = Map[String, Json] => A

  implicit val vFunctor: Functor[V] = new Functor[V] {
    override def map[A, B](fa: Map[String, Json] => A)(f: A => B): Map[String, Json] => B = {
      { data: Map[String, Json] =>
        f(fa(data))
      }
    }
  }

  val unfullreadGen: SingleFoldableChannel[Map[String, Json] => DataPileContent, V] = DataPile.transformTree {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
        .mapTo {
          case (jsonReader :: property :: defaultOpt :: HNil, data) =>
            val tran: Map[String, Json] => AtomicValueImpl[path.DataType] = { sourceData: Map[String, Json] =>
              sourceData.get(property.proName) match {
                case Some(json) =>
                  json.as[jsonReader.DataType](jsonReader.reader) match {
                    case Right(data) =>
                      set(data)
                    case Left(_) =>
                      val ifEmptyData = mergeDefault(defaultOpt, data)
                      //ifEmptyData
                      //ifEmptyData.map(set).getOrElse(AtomicValueImpl.empty)
                      setOpt(ifEmptyData)
                  }
                case None =>
                  //防止前端因为值为 null 或 undefined 而删除了该属性
                  Json.Null.as[jsonReader.DataType](jsonReader.reader) match {
                    case Right(data) =>
                      set(data)
                    case Left(_) =>
                      val ifEmptyData = mergeDefault(defaultOpt, data)
                      setOpt(ifEmptyData)
                  }
              }
            }

            tran: (Map[String, Json] => AtomicValue)
        }
    }.aa
  } { (readlerList, atomicGen) =>
    { sourceData: Map[String, Json] =>
      atomicGen.toContent(readlerList.map(_.apply(sourceData)))
    }
  }.withSyntax(new PileSyntaxFunctor[Map[String, Json] => DataPileContent, V] {
    override def pileMap[U](a: Map[String, Json] => DataPileContent, pervious: DataPileContent => U): Map[String, Json] => U = {
      { sourceData: Map[String, Json] =>
        pervious(a(sourceData))
      }
    }
  }).withFunctor(vFunctor)

  val jsonObjectWriteGen: SingleInputChannel[OBJV[JsonObject]] = DataPile.transformTree {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[JsonWriter] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
        .mapTo {
          case (jsonWriter :: property :: defaultOpt :: HNil, data) => {
            val exportData = mergeDefault(defaultOpt, data)
            //val eachColumnData: path.DataType = exportData.getOrElse(throw new Exception(s"字段 ${property.proName} 未被定义"))
            implicit val writerJ = jsonWriter.writer
            exportData.map(s => property.proName -> s.asJson)
          }
        }
    }.aa
  } { (jsonTupleList, atomicGen) =>
    OBJV { jobj: JsonObject =>
      jsonTupleList.collect { case Some(s) => s }.foldLeft(jobj) {
        case (newJobj, (key, value)) =>
          newJobj.add(key, value)
      }
    }
  }

  trait OBJV[A] extends (JsonObject => A) {
    override def apply(v1: JsonObject): A
  }

  object OBJV {
    def apply[T](cv: JsonObject => T): OBJV[T] = new OBJV[T] {
      override def apply(v1: JsonObject): T = cv(v1)
    }
  }

  implicit val objFunctor: Functor[OBJV] = new Functor[OBJV] {
    override def map[A, B](fa: OBJV[A])(f: A => B): OBJV[B] = {
      { data: JsonObject =>
        f(fa(data))
      }
    }
  }

  val jsonObjectReadGen: SingleFoldableChannel[OBJV[DataPileContent], OBJV] = DataPile.transformTree {
    new AtomicQuery(_) {
      val aa = withRep(needAtomic[JsonReader] :: needAtomic[FProperty] :: needAtomicOpt[DefaultValue] :: FANil)
        .mapTo {
          case (jsonReader :: property :: defaultOpt :: HNil, data) =>
            OBJV { sourceData: JsonObject =>
              (sourceData(property.proName) match {
                case Some(json) =>
                  json.as[jsonReader.DataType](jsonReader.reader) match {
                    case Right(data) =>
                      set(data)
                    case Left(_) =>
                      val ifEmptyData = mergeDefault(defaultOpt, data)
                      //ifEmptyData
                      //ifEmptyData.map(set).getOrElse(AtomicValueImpl.empty)
                      setOpt(ifEmptyData)
                  }
                case None =>
                  //防止前端因为值为 null 或 undefined 而删除了该属性
                  Json.Null.as[jsonReader.DataType](jsonReader.reader) match {
                    case Right(data) =>
                      set(data)
                    case Left(_) =>
                      val ifEmptyData = mergeDefault(defaultOpt, data)
                      setOpt(ifEmptyData)
                  }
              }): AtomicValue
            }
        }
    }.aa
  } { (readlerList, atomicGen) =>
    OBJV { sourceData: JsonObject =>
      atomicGen.toContent(readlerList.map(_.apply(sourceData)))
    }
  }.withSyntax(new PileSyntaxFunctor[OBJV[DataPileContent], OBJV] {
    override def pileMap[U](a: OBJV[DataPileContent], pervious: DataPileContent => U): OBJV[U] = {
      OBJV { sourceData: JsonObject =>
        pervious(a(sourceData))
      }
    }
  }).withFunctor(objFunctor)

}