package indicator.rw

import aaaa.FilterWrapper1111
import bbbb.FRep
import net.scalax.fsn.core.FColumn
import indicator.rw.utils.rw._
import io.circe.{Decoder, Encoder}
import net.scalax.fsn.core.FAtomic
import net.scalax.fsn.model.StaticManyGen
import org.xarcher.cpoi.ReadableCellOperationAbs
import slick.lifted.{FlatShapeLevel, Shape}

import scala.concurrent.Future
import scala.reflect.runtime.universe._

object In {

  def subUbw[T](cols: FColumn*): List[FAtomic[T]] = new SubUbw[T] {
    override val subCols = cols.toList
  } :: Nil

  def subUbw[T](cols: List[FColumn]): List[FAtomic[T]] = new SubUbw[T] {
    override val subCols = cols
  } :: Nil

  def property[E](name: String) = new FProperty[E] {
    override val proName = name
  }

  def jRead[T](implicit decoder: Decoder[T]): List[FAtomic[T]] = List(new JsonReader[T] {
    override type JsonType = T
    override val reader = decoder
    override val convert = identity[T] _
  })

  def default[T](data: T): List[FAtomic[T]] = List(new DefaultValue[T] {
    override val value = data
  })

  def jWrite[T](
    implicit
    encoder: Encoder[T],
    weakTypeTag: WeakTypeTag[T]//,
    //selectRender1: SelectRender[T],
    //retrieveRender1: RetrieveRender[T],
    //inputRender1: InputRender[T]
  ): List[FAtomic[T]] = List(new JsonWriter[T] {
    override type JsonType = T
    //override val selectRender = selectRender1
    //override val retrieveRender = retrieveRender1
    //override val inputRender = inputRender1
    override val writer = encoder
    override val convert = identity[T] _
    override val typeTag = weakTypeTag
  })

  def create[S, D, T](sourceCol: FRep[S])(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[FAtomic[D]] = List(new SlickCreate[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T

    override val mainCol = sourceCol
    override val mainShape = shape
    override val convert = identity[D] _
    override val reverseConvert = identity[D] _
  })

  def autoInc[T]: List[FAtomic[T]] = List(new AutoInc[T] {
    override val isAutoInc = true
  })

  /*def retrieve[S, D, T](sourceCol: FRep[S])(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[FAtomic[D]] = List(new SlickRetrieve[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T

    override val mainCol = sourceCol
    override val mainShape = shape
    override val primaryGen = Option.empty[(Map[String, Json], String) => FilterWrapper[T]]
    override val convert = identity[D] _
  })

  def update[S, D, T, U, V](sourceCol: FRep[S])(
    implicit
    mainShape1: Shape[_ <: FlatShapeLevel, S, D, T],
    updateShape1: Shape[_ <: FlatShapeLevel, T, U, V]
  ): List[FAtomic[U]] = List(new SlickUpdate[U] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T
    override type USlickType = U
    override type UTargetType = V

    override val mainCol = sourceCol
    override val mainShape = mainShape1
    override val updateShape = updateShape1
    override val primaryGen = Option.empty[(Map[String, Json], String) => FilterWrapper[T]]
    override val convert = identity[U] _
  })

  def delete[S, D, T](sourceCol: FRep[S])(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[FAtomic[D]] = List(new SlickDelete[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T

    override val mainCol = sourceCol
    override val mainShape = shape
    override val primaryGen = Option.empty[(Map[String, Json], String) => FilterWrapper[T]]
  })*/

  def oneTOneR[S, D, T](sourceCol: FRep[S])(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T], filterGen: FilterWrapper1111[T, D]): List[FAtomic[D]] = List(new OneToOneRetrieve[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T
    override type FilterData = D

    override val mainCol = sourceCol
    override val mainShape = shape
    override val primaryGen = filterGen
    override val filterConvert = identity[D] _
  })

  def oneTOneU[S, D, T/*, U*/](sourceCol: FRep[S])(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    //shape2: Shape[_ <: FlatShapeLevel, T, D, U],
    filterGen: FilterWrapper1111[T, D]
  ): List[FAtomic[D]] = List(new OneToOneUpdate[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T
    //override type USlickType = D
    //override type UTargetType = U
    override type FilterData = D

    override val mainCol = sourceCol
    override val mainShape = shape
    //override val updateShape = shape2
    override val primaryGen = filterGen
    override val convert = identity[D] _
    override val filterConvert = identity[D] _
  })

  def oneTOneC[S, D, T](sourceCol: FRep[S])(implicit shape: Shape[_ <: FlatShapeLevel, S, D, T]): List[FAtomic[D]] = List(new OneToOneCrate[D] {
    override type SourceType = S
    override type SlickType = D
    override type TargetType = T

    override val mainCol = sourceCol
    override val mainShape = shape
    override val convert = identity[D] _
  })

  def oneTOne[S, D, T/*, U*/](sourceCol: FRep[S])(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    //shape2: Shape[_ <: FlatShapeLevel, T, D, U],
    filterGen: FilterWrapper1111[T, D]
  ): List[FAtomic[D]] = List(
    new OneToOneRetrieve[D] {
      override type SourceType = S
      override type SlickType = D
      override type TargetType = T
      override type FilterData = D

      override val mainCol = sourceCol
      override val mainShape = shape
      override val primaryGen = filterGen
      override val filterConvert = identity[D] _
    },
    new OneToOneUpdate[D] {
      override type SourceType = S
      override type SlickType = D
      override type TargetType = T
      //override type USlickType = D
      //override type UTargetType = U
      override type FilterData = D

      override val mainCol = sourceCol
      override val mainShape = shape
      //override val updateShape = shape2
      override val primaryGen = filterGen
      override val convert = identity[D] _
      override val filterConvert = identity[D] _
    },
    new OneToOneCrate[D] {
      override type SourceType = S
      override type SlickType = D
      override type TargetType = T

      override val mainCol = sourceCol
      override val mainShape = shape
      override val convert = identity[D] _
    }
  )

  def staticMany[E](staticMany1: Future[List[StaticManyGen[E]]]): List[FAtomic[E]] = List(new StaticMany[E] {
    val staticMany = staticMany1
  })

  def excelRead[E](implicit reader1: ReadableCellOperationAbs[E]): List[FAtomic[E]] = List(new PoiReader[E] {
    override type PoiType = E
    override val reader = reader1
    override val convert = identity[E] _
  })

}