package indicator.rw.utils.rw

import aaaa.FilterWrapper1111
import bbbb.FRep
import io.circe.{Decoder, Encoder}
import net.scalax.fsn.core.{FAtomic, FColumn}
import net.scalax.fsn.slick.model.StaticManyGen
import org.xarcher.cpoi.{ReadableCellOperationAbs, WriteableCellOperationAbs}
import slick.lifted.{ColumnOrdered, FlatShapeLevel, Shape}

import scala.language.existentials
import scala.language.implicitConversions
import scala.concurrent.Future
import scala.reflect.runtime.universe._

trait SubUbw[E] extends FAtomic[E] {
  val subCols: List[FColumn]
}

trait JsonReader[E] extends FAtomic[E] {

  type JsonType
  type DataType = E

  val reader: Decoder[JsonType]
  val convert: JsonType => DataType

}

trait DefaultValue[E] extends FAtomic[E] {
  val value: E
}

trait JsonWriter[E] extends FAtomic[E] {

  type JsonType
  type DataType = E
  val writer: Encoder[JsonType]
  val convert: DataType => JsonType
  val typeTag: WeakTypeTag[JsonType]

}

trait SlickCreate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val convert: SlickType => DataType
  val reverseConvert: DataType => SlickType

}

case class SCreate[S, D, T, E](
  override val mainCol: FRep[S],
  override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
  override val convert: D => E,
  override val reverseConvert: E => D
) extends SlickCreate[E] {
  type SourceType = S
  type SlickType = D
  type TargetType = T
}

trait AutoInc[E] extends FAtomic[E] {
  val isAutoInc: Boolean
}

trait SlickRetrieve[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E
  type FilterData

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: Option[FilterWrapper1111[TargetType, FilterData]]
  val convert: SlickType => DataType
  val filterConvert: DataType => FilterData

}

case class SRetrieve[S, D, T, A, E](
  override val mainCol: FRep[S],
  override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
  override val primaryGen: Option[FilterWrapper1111[T, A]],
  override val convert: D => E,
  override val filterConvert: E => A
) extends SlickRetrieve[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type FilterData = A
}

trait SlickUpdate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type FilterData

  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: Option[FilterWrapper1111[TargetType, FilterData]]
  val convert: DataType => SlickType
  val filterConvert: DataType => FilterData

}

case class SUpdate[S, D, T, C, E](
                                  override val mainCol: FRep[S],
                                  override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
                                  override val primaryGen: Option[FilterWrapper1111[T, C]],
                                  override val convert: E => D,
                                  override val filterConvert: E => C
) extends SlickUpdate[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type FilterData = C
}

trait SlickDelete[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E
  type FilterData

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: Option[FilterWrapper1111[TargetType, FilterData]]
  val filterConvert: DataType => FilterData

}

case class SDelete[S, D, T, U, E](
  override val mainCol: FRep[S],
  override val mainShape: Shape[_ <: FlatShapeLevel, S, D, T],
  override val primaryGen: Option[FilterWrapper1111[T, U]],
  override val filterConvert: E => U
                                 ) extends SlickDelete[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
  override type FilterData = U
}

trait OneToOneRetrieve[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type FilterData
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val primaryGen: FilterWrapper1111[TargetType, FilterData]
  val filterConvert: DataType => FilterData

}

trait OneToOneCrate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val convert: DataType => SlickType

}

trait OneToOneUpdate[E] extends FAtomic[E] {

  type SourceType
  type SlickType
  type TargetType
  //type USlickType
  //type UTargetType
  type FilterData
  type DataType = E

  val mainCol: FRep[SourceType]
  val mainShape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  //val updateShape: Shape[_ <: FlatShapeLevel, TargetType, USlickType, UTargetType]
  val primaryGen: FilterWrapper1111[TargetType, FilterData]
  //val convert: DataType => USlickType
  val convert: DataType => SlickType
  val filterConvert: DataType => FilterData

}

trait StaticMany[E] extends FAtomic[E] {

  type DataType = E

  val staticMany: Future[List[StaticManyGen[DataType]]]

}

trait SlickSelect[E] extends FAtomic[E] {
  type SourceType
  type SlickType
  type TargetType
  type DataType = E

  val shape: Shape[_ <: FlatShapeLevel, SourceType, SlickType, TargetType]
  val outConvert: SlickType => DataType
  val outCol: SourceType
  val colToOrder: Option[TargetType => ColumnOrdered[_]]
}

case class SSelect[S, D, T, E](
  override val shape: Shape[_ <: FlatShapeLevel, S, D, T],
  override val outConvert: D => E,
  override val outCol: S,
  override val colToOrder: Option[T => ColumnOrdered[_]]
) extends SlickSelect[E] {
  override type SourceType = S
  override type SlickType = D
  override type TargetType = T
}

trait OrderNullsLast[E] extends FAtomic[E] {
  val isOrderNullsLast: Boolean
}

trait DefaultDesc[E] extends FAtomic[E] {
  val isDefaultDesc: Boolean
}

trait InRetrieve[E] extends FAtomic[E] {
  val isInRetrieve: Boolean
}

trait OrderTargetName[E] extends FAtomic[E] {
  val orderTargetName: String
}

trait PoiReader[E] extends FAtomic[E] {
  type PoiType
  type DataType = E

  val reader: ReadableCellOperationAbs[PoiType]
  val convert: PoiType => DataType
}

trait PoiWriter[E] extends FAtomic[E] {
  type PoiType
  type DataType = E

  val writer: WriteableCellOperationAbs[PoiType]
  val convert: DataType => PoiType
}