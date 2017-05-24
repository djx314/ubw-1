package net.scalax.fsn.mix.helpers

import net.scalax.fsn.core.{ FAtomic, FAtomicPathImpl }
import net.scalax.fsn.slick.atomic._
import net.scalax.fsn.slick.helpers.FilterWrapper
import slick.lifted.{ FlatShapeLevel, Shape }

case class SCRUD[S, D, T, E](
    create: SCreate[S, D, T, E],
    retrieve: SRetrieve[S, D, T, D, E],
    update: SUpdate[S, D, T, D, E],
    delete: SDelete[S, D, T, D, E],
    //jRead: JsonReader[E],
    //jWrite: JsonWriter[E],
    isAutoInc: Boolean
) extends FAtomicPathImpl[E] { self =>
  def primary(implicit priFilter: FilterWrapper[T, D]): SCRUD[S, D, T, E] = {
    this.copy(
      retrieve = retrieve.copy(primaryGen = Option(priFilter)),
      update = update.copy(primaryGen = Option(priFilter)),
      delete = delete.copy(primaryGen = Option(priFilter))
    )
  }

  def autoInc: SCRUD[S, D, T, E] = {
    this.copy(isAutoInc = true)
  }

  def result: List[FAtomic[E]] = {
    List(
      create,
      retrieve,
      update,
      delete,
      //jRead,
      //jWrite,
      new AutoInc[E] {
        override val isAutoInc = self.isAutoInc
      }
    )
  }

  override type DataType = E
  override val atomics = result
}

object SCRUD {

  def in[S, D, T](repLike: S, owner1: Any)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T] //,
  //encoder: Encoder[D],
  //decoder: Decoder[D],
  //weakTypeTag: WeakTypeTag[D]
  ): SCRUD[S, D, T, D] = {
    val sCreate = SCreate[S, D, T, D](
      mainCol = repLike,
      owner = owner1,
      mainShape = shape,
      convert = identity[D] _,
      reverseConvert = identity[D] _
    )

    val sRetrieve = SRetrieve[S, D, T, D, D](
      mainCol = repLike,
      owner = owner1,
      mainShape = shape,
      primaryGen = None,
      convert = identity[D] _,
      filterConvert = identity[D] _
    )

    val sUpdate = SUpdate[S, D, T, D, D](
      mainCol = repLike,
      owner = owner1,
      mainShape = shape,
      primaryGen = None,
      convert = identity[D] _,
      filterConvert = identity[D] _
    )

    val sDelete = SDelete[S, D, T, D, D](
      mainCol = repLike,
      owner = owner1,
      mainShape = shape,
      primaryGen = None,
      filterConvert = identity[D] _
    )

    /*val jsonReader = new JsonReader[D] {
      override val convert = identity[D] _
      override type JsonType = D
      override val reader = decoder
    }

    val jsonWriter = new JsonWriter[D] {
      override val convert = identity[D] _
      override type JsonType = D
      override val writer = encoder
      override val typeTag = weakTypeTag
    }*/

    SCRUD[S, D, T, D](
      create = sCreate,
      retrieve = sRetrieve,
      update = sUpdate,
      delete = sDelete,
      //jRead = jsonReader,
      //jWrite = jsonWriter,
      isAutoInc = false
    )
  }

  /*case class Embber[S, D, T](repLike: S, owner1: Any)(
      shape: Shape[_ <: FlatShapeLevel, S, D, T]
  ) {
    def convert[E](out: D => E)(in: E => D)(
      implicit
      encoder: Encoder[E],
      decoder: Decoder[E],
      weakTypeTag: WeakTypeTag[E]
    ): SCRUD[S, D, T, E] = {
      val sCreate = SCreate[S, D, T, E](
        mainCol = repLike,
        owner = owner1,
        mainShape = shape,
        convert = out,
        reverseConvert = in
      )

      val sRetrieve = SRetrieve[S, D, T, D, E](
        mainCol = repLike,
        owner = owner1,
        mainShape = shape,
        primaryGen = None,
        convert = out,
        filterConvert = in
      )

      val sUpdate = SUpdate[S, D, T, D, E](
        mainCol = repLike,
        owner = owner1,
        mainShape = shape,
        primaryGen = None,
        convert = in,
        filterConvert = in
      )

      val sDelete = SDelete[S, D, T, D, E](
        mainCol = repLike,
        owner = owner1,
        mainShape = shape,
        primaryGen = None,
        filterConvert = in
      )

      val jsonReader = new JsonReader[E] {
        override val convert = identity[E] _
        override type JsonType = E
        override val reader = decoder
      }

      val jsonWriter = new JsonWriter[E] {
        override val convert = identity[E] _
        override type JsonType = E
        override val writer = encoder
        override val typeTag = weakTypeTag
      }

      SCRUD[S, D, T, E](
        create = sCreate,
        retrieve = sRetrieve,
        update = sUpdate,
        delete = sDelete,
        jRead = jsonReader,
        jWrite = jsonWriter,
        isAutoInc = false
      )
    }
  }

  def inExt[S, D, T, A](repLike: S, owner1: RelationalProfile#Table[_])(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T]
  ): Embber[S, D, T] = {
    Embber(repLike, SlickUtils.getTableIdFromTable(owner1))(shape)
  }

  def inExt[S, D, T, A](repLike: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T]
  ): Embber[S, D, T] = {
    Embber(repLike, SlickUtils.getTableIdFromCol(repLike)(shape))(shape)
  }

  def inExt[S <: Rep[_], D, T, A](repLike: FRep[S])(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T]
  ): Embber[S, D, T] = {
    Embber(repLike.rep, SlickUtils.getTableIdFromTable(repLike.owner))(shape)
  }*/

}
/*case class Select[S, D, T, E](
 select: SSelect[S, D, T, E],
 jsonWriter: JsonWriter[E],
 orderNullsLast: Boolean,
 defaultDesc: Boolean,
 inRetrieve: Boolean,
 orderTargetName: Option[String]
) { self =>

  def hidden = this.copy(inRetrieve = false)

  def orderTarget(name: String) = this.copy(orderTargetName = Option(name))

  def desc = this.copy(defaultDesc = true)
  def asc = this.copy(defaultDesc = false)

  def nullsLast = this.copy(orderNullsLast = true)
  def nullsFirst = this.copy(orderNullsLast = false)

  def order(implicit cv: T => ColumnOrdered[_]) = {
    this.copy(select = this.select.copy(colToOrder = Option(cv)))
  }

  def result: List[FAtomic[E]] = {
    List(
      select,
      jsonWriter,
      new InRetrieve[E] {
        override val isInRetrieve = self.inRetrieve
      },
      new DefaultDesc[E] {
        override val isDefaultDesc = self.defaultDesc
      },
      new OrderNullsLast[E] {
        override val isOrderNullsLast = self.orderNullsLast
      }
    ) ::: self.orderTargetName.map(s => new OrderTargetName[E] {
      override val orderTargetName = s
    }).toList
  }

}

object Select {

  case class Embber[S, D, T](repLike: S)(
    shape1: Shape[_ <: FlatShapeLevel, S, D, T]
  ) {
    def convert[E](out: D => E)(
      implicit
      encoder1: Encoder[E],
      weakTypeTag: WeakTypeTag[E]
    ) = {
      val jsonWriter = new JsonWriter[E] {
        override val convert = identity[E] _
        override type JsonType = E
        override val writer = encoder1
        override val typeTag = weakTypeTag
      }

      val sSelect = SSelect(
        shape = shape1,
        outConvert = out,
        outCol = repLike,
        colToOrder = None
      )

      Select(
        sSelect,
        jsonWriter,
        true,
        true,
        true,
        None
      )
    }
  }

  def outExt[S, D, T](repLike: S)(
    implicit
    shape1: Shape[_ <: FlatShapeLevel, S, D, T]/*,
    encoder: Encoder[D],
    weakTypeTag: WeakTypeTag[D]*/
  ): Embber[S, D, T] = {
    Embber(repLike)(shape1)
  }
  /*def out[S, D, T](repLike: S)(
    implicit
    shape1: Shape[_ <: FlatShapeLevel, S, D, T],
    encoder: Encoder[D],
    weakTypeTag: WeakTypeTag[D]
  ): Select[S, D, T, D] = {
    val jsonWriter = new JsonWriter[D] {
      override val convert = identity[D] _
      override type JsonType = D
      override val writer = encoder
      override val typeTag = weakTypeTag
    }

    val sSelect = SSelect(
      shape = shape1,
      outConvert = identity[D] _,
      outCol = repLike,
      colToOrder = None
    )

    Select(
      sSelect,
      jsonWriter,
      true,
      true,
      true,
      None
    )

  }*/
}*/ 