/*package indicator.rw.utils.rw2

import indicator.rw.utils.SlickQueryBindImpl
import indicator.rw.utils.rw._
import net.scalax.fsn.core._
import net.scalax.fsn.slick_common.{ColumnOrder, SlickPage, SlickParam, SlickRange}
import slick.basic.BasicProfile
import slick.dbio.{DBIO, NoStream}
import slick.lifted._
import scala.language.existentials
import scala.concurrent.ExecutionContext

trait JsonMonad {

  import scalaz._

  implicit val slickJsonConvertMonoId: Monoid[SlickReader] = new Monoid[SlickReader] {

    override def zero: SlickReader = SReader(
      sourceCol = (),
      mainShape = implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]],
      listGen = (s: Unit) => Nil,
      orderGen = Map.empty[String, Unit => ColumnOrdered[_]],
      orderTargetGen = Map.empty[String, String]
    )

    override def append(f1: SlickReader, f2: => SlickReader): SlickReader = {
      val f2Case = f2
      val newShape = Shape.tuple2Shape(f1.mainShape, f2Case.mainShape)

      val sourceOrderGen: Map[String, ((f1.TargetColumn, f2Case.TargetColumn)) => ColumnOrdered[_]] =
        for {
          (key, gen) <- f1.orderGen
        } yield {
          val newGen = (s: (f1.TargetColumn, f2Case.TargetColumn)) => {
            val (sTargetCol, _) = s
            gen(sTargetCol)
          }
          key -> newGen
        }

      val appendOrderGen: Map[String, ((f1.TargetColumn, f2Case.TargetColumn)) => ColumnOrdered[_]] =
        for {
          (key, gen) <- f2Case.orderGen
        } yield {
          val newGen = (s: (f1.TargetColumn, f2Case.TargetColumn)) => {
            val (_, aTargetCol) = s
            gen(aTargetCol)
          }
          key -> newGen
        }

      val listGen: ((f1.DataType, f2Case.DataType)) => List[FColumn] = { s =>
        f1.listGen(s._1) ::: f2Case.listGen(s._2)
      }

      SReader(f1.sourceCol -> f2Case.sourceCol, newShape, listGen, sourceOrderGen ++ appendOrderGen, f1.orderTargetGen ++ f2Case.orderTargetGen)
    }
  }

}

trait SlickReader {

  type SourceColumn
  type DataType
  type TargetColumn

  val orderGen: Map[String, TargetColumn => ColumnOrdered[_]]
  val orderTargetGen: Map[String, String]
  val sourceCol: SourceColumn

  val mainShape: Shape[_ <: FlatShapeLevel, SourceColumn, DataType, TargetColumn]

  val listGen: DataType => List[FColumn]

}

case class SReader[S, D, T](
                             override val sourceCol: S,
                             override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
                             override val listGen: D => List[FColumn],
                             override val orderGen: Map[String, T => ColumnOrdered[_]],
                             override val orderTargetGen: Map[String, String]
                           ) extends SlickReader {

  override type SourceColumn = S
  override type DataType = D
  override type TargetColumn = T

}

object OutSelectConvert {

  def convert(columns: FColumn): SlickReader = {
    val slickSelect = FColumn.find(columns) { case s: SlickSelect[columns.DataType] => s }
    val isOrderNullsLast = FColumn.findOpt(columns) { case s: OrderNullsLast[columns.DataType] => s }.map(_.isOrderNullsLast).getOrElse(true)
    val orderTargetName = FColumn.findOpt(columns) { case s: OrderTargetName[columns.DataType] => s }.map(_.orderTargetName)
    val property = FColumn.find(columns) { case s: FProperty[columns.DataType] => s }

    def slickReaderGen: SReader[slickSelect.SourceType, slickSelect.SlickType, slickSelect.TargetType] = if (isOrderNullsLast)
      SReader(
        slickSelect.outCol,
        slickSelect.shape,
        { s: slickSelect.SlickType =>
          List(FsnColumn(columns.cols/*, property.proName*/, Option(slickSelect.outConvert(s))))
        },
        slickSelect.colToOrder.map(s => Map(property.proName -> ((t: slickSelect.TargetType) => s(t).nullsLast))).getOrElse(Map()),
        orderTargetName.map(s => Map(property.proName -> s)).getOrElse(Map())
      )
    else
      SReader(
        slickSelect.outCol,
        slickSelect.shape,
        { s: slickSelect.SlickType =>
          List(FsnColumn(columns.cols/*, property.proName*/, Option(slickSelect.outConvert(s))))
        },
        slickSelect.colToOrder.map(s => Map(property.proName -> ((t: slickSelect.TargetType) => s(t).nullsFirst))).getOrElse(Map()),
        orderTargetName.map(s => Map(property.proName -> s)).getOrElse(Map())
      )

    slickReaderGen
  }

  def extraSubCol(columns: List[FColumn]): List[FColumn] = {
    val extraColumns = columns.map { s =>
      val subCols = FColumn.findOpt(s) { case t: SubUbw[s.DataType] => t }
      subCols match {
        case Some(t) => t.subCols
        case _ => List(s)
      }
    }.flatten
    if (extraColumns.exists(s => FColumn.findOpt(s) { case t: SubUbw[s.DataType] => t }.isDefined)) {
      extraSubCol(extraColumns)
    } else {
      extraColumns
    }
  }

}

object SlickJsonFShape extends JsonMonad {

  import scalaz.Monoid

  def encode(columns: List[FColumn], wQuery: SlickQueryBindImpl): JsonQuery = {
    val slickJsonMono = implicitly[Monoid[SlickReader]]

    val fv = columns.map(OutSelectConvert.convert).foldLeft(slickJsonMono.zero)((s, t) => slickJsonMono.append(s, t))
    val fvQuery = Query(fv.sourceCol)(fv.mainShape)
    val mapQuery = wQuery.bind(fvQuery)
    val jsonSortTargetGen: Map[String, fv.TargetColumn => ColumnOrdered[_]] = fv.orderTargetGen.map { case (key, value) =>
      key -> fv.orderGen.get(value).getOrElse(throw new Exception(s"$key 需要映射 $value 的排序方案，但找不到 $value 对应的列的排序"))
    } ++ fv.orderGen
    new JsonQuery {
      override type JsonE = fv.TargetColumn
      override type JsonU = fv.DataType
      override val uQuery = mapQuery
      override val render = fv.listGen
      override val sortMap = jsonSortTargetGen
    }
  }

}

trait JsonQuery {

  type JsonE
  type JsonU

  val uQuery: Query[JsonE, JsonU, Seq]
  val render: JsonU => List[FColumn]
  val sortMap: Map[String, JsonE => ColumnOrdered[_]]

  def jsonResult(
                  implicit
                  jsonEv: Query[_, JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[JsonU], JsonU],
                  repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
                  ec: ExecutionContext
                ): SlickParam => DBIO[(List[List[FColumn]], Int)] = {
    jsonResult(Nil)
  }

  def jsonResult(orderColumn: String, isDesc: Boolean = true)(
    implicit
    jsonEv: Query[_, JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[JsonU], JsonU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[FColumn]], Int)] = {
    jsonResult(List(ColumnOrder(orderColumn, isDesc)))
  }

  def jsonResult(defaultOrders: List[ColumnOrder])(
    implicit
    jsonEv: Query[_, JsonU, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[JsonU], JsonU],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[(List[List[FColumn]], Int)] = {
    (slickParam: SlickParam) => CommonResult.commonResult(defaultOrders, uQuery, render, sortMap).apply(slickParam)
  }

}

object CommonResult {

  type CommonRType[T] = (/*List[PropertyInfo],*/List[T], Int)

  def commonResult[E, U, T](defaultOrders: List[ColumnOrder], query: Query[E, U, Seq], modelConvert: U => T, sortMap: Map[String, E => ColumnOrdered[_]]/*, properties: List[PropertyInfo]*/)(
    implicit
    jsonEv: Query[E, U, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[U], U],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): SlickParam => DBIO[CommonRType[T]] = {
    val mappedQuery = query

    val result: SlickParam => DBIO[CommonRType[T]] = slickParam => {
      val autualOrders = defaultOrders ::: slickParam.orders
      val baseQuery = {
        autualOrders.foldLeft(mappedQuery) { case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
          sortMap.get(eachOrderName) match {
            case Some(convert) =>
              eachQuery.sortBy { s =>
                val colOrder = convert(s)

                if (eachIsDesc)
                  colOrder.desc
                else
                  colOrder.asc
              }
            case _ =>
              eachQuery
          }
        }
      }

      slickParam match {
        case SlickParam(_, Some(SlickRange(drop1, Some(take1))), Some(SlickPage(pageIndex1, pageSize1))) =>
          val startCount = Math.max(0, drop1)
          val pageIndex = Math.max(0, pageIndex1)
          val pageSize = Math.max(0, pageSize1)

          val dropQuery = mappedQuery.drop(startCount)

          (for {
            sum <- dropQuery.size.result
          } yield {
            val pageStart = startCount + pageIndex * pageSize
            val pageEnd = pageStart + pageSize
            val endCount = Math.min(take1, startCount + sum)
            val autalStart = Math.max(pageStart, startCount)
            val autalEnd = Math.min(pageEnd, endCount)
            val autalLimit = Math.max(0, autalEnd - autalStart)

            val limitQuery = baseQuery.drop(startCount).drop(pageIndex * pageSize).take(autalLimit)

            limitQuery.result.map(s => {
              val dataGen = s.toList.map(t => {
                modelConvert(t)
              })
              (/*properties,*/dataGen, endCount - startCount)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, Some(take))), None) =>
          val dropQuery = mappedQuery.drop(drop)
          //val takeQuery = dropQuery.take(take)

          baseQuery.drop(drop).take(take - drop).result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (/*properties,*/dataGen, s.size)
          })

        case SlickParam(_, Some(SlickRange(drop1, None)), Some(SlickPage(pageIndex1, pageSize1))) =>
          val startCount = Math.max(0, drop1)
          val pageIndex = Math.max(0, pageIndex1)
          val pageSize = Math.max(0, pageSize1)

          val dropQuery = mappedQuery.drop(startCount)

          (for {
            sum <- dropQuery.size.result
          } yield {

            val limitQuery = baseQuery.drop(startCount).drop(pageIndex * pageSize).take(pageSize)

            limitQuery.result.map(s => {
              val dataGen = s.toList.map(t => {
                modelConvert(t)
              })
              (/*properties,*/dataGen, sum)
            })
          })
            .flatMap(s => s)

        case SlickParam(_, Some(SlickRange(drop, None)), None) =>
          baseQuery.drop(drop).result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (/*properties,*/dataGen, s.size)
          })

        case SlickParam(_, None, Some(SlickPage(pageIndex, pageSize))) =>
          val dropQuery = baseQuery.drop(pageIndex * pageSize)
          val takeQuery = dropQuery.take(pageSize)

          for {
            sum <- mappedQuery.size.result
            s <- takeQuery.result
          } yield {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (/*properties,*/dataGen, sum)
          }
        case _ =>
          baseQuery.result.map(s => {
            val dataGen = s.toList.map(t => {
              modelConvert(t)
            })
            (/*properties,*/dataGen, s.size)
          })
      }
    }

    result

  }

}*/